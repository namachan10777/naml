type ty =
    | TNever
    | TInt
    | TBool
    | TStr
    | TFun of ty * ty
    | TTuple of ty list
    | Poly of int
    | TyVar of int (* arrayに対するindexとして持つ *)
    | TVariant of ty list * Id.t
[@@deriving show]
and ty_var_t = Just of ty * int list | Unknown of int * int * int list [@@deriving show]

(* 型推論を実装する際に問題となるのは不明な型をどう扱うかである。
 * 不明ではあってもunifyによって不明なまま単一化されることがありうる。
 * ナイーブにrefで実装するとこの単一となる制約を守りきれない（無限段必要になる）
 * 一つの解決策として、「自分自身を含む同一制約を持つ全てのの不明な方への参照のリスト」を保持し、
 * コレを用いて書き換えるという方法がある。ただしこれは循環参照を作成するためppx_derivingで表示できず=で比較も出来ない。
 * 双方ともに単純な深さ優先でポインタをたどるためである。
 * もう一つの解決策としてArrayを使う方法がある。これは実質的にはRAMのエミュレートであるが、
 * 実体の場所が自明にグローバルなのでバグを起こしづらい。また=やppx_deriving.showがそのまま使えるのが利点である。
 * 今回は後者を採用している。
 *)

exception UnifyError
exception TypingError

type pat_t =
    | PInt of int * Lex.pos_t
    | PBool of bool * Lex.pos_t
    | PVar of Id.t * ty * Lex.pos_t
    | PTuple of (pat_t * ty) list * Lex.pos_t
    | As of pat_t list * ty * Lex.pos_t
    | Or of pat_t * pat_t list * ty * Lex.pos_t
    | PCtorApp of Id.t * (pat_t * ty) list * ty * Lex.pos_t
[@@deriving show]

type t =
    | Never
    | Int of int * Lex.pos_t
    | Bool of bool * Lex.pos_t
    | Var of Id.t * ty * Lex.pos_t
    | CtorApp of Id.t * Lex.pos_t * (t * ty) list * ty
    | Tuple of (t * ty) list * Lex.pos_t
    | If of t * t * t * ty * Lex.pos_t
    | Let of ((pat_t * ty) * Lex.pos_t * (t * ty)) list * (t * ty)
    | LetRec of (Id.t * Lex.pos_t * (t * ty)) list * (t * ty)
    | Fun of (Id.t * ty) * (t * ty) * Lex.pos_t
    | Match of (t * ty) * ((pat_t * ty) * Lex.pos_t * t * t) list * ty
    | App of (t * ty) * (t * ty) * Lex.pos_t
[@@deriving show]

let store = ref @@ Array.init 2 (fun i -> Unknown (0, i, [i]))
type tenv_t = ty_var_t array [@@deriving show]

let count = ref 0

let init () =
    store := Array.init 2 (fun i -> Unknown (0, i, [i]));
    count := 0

let fresh level =
    let arr_len = Array.length !store in
    if !count >= arr_len then (
      store :=
        Array.concat
          [!store; Array.init arr_len (fun i -> Unknown (0, i+arr_len, [i + arr_len]))] ;
      let idx = !count in
      count := 1 + !count ;
      (* levelは初期で全て0なので正しいlevelの不明型を代入する必要がある *)
      !store.(idx) <- Unknown(level, idx, [idx]);
      TyVar idx )
    else (
      let idx = !count in
      count := 1 + !count ;
      (* levelは初期で全て0なので正しいlevelの不明型を代入する必要がある *)
      !store.(idx) <- Unknown(level, idx, [idx]);
      TyVar idx)

exception CyclicType

let rec collect_tags_of_unknown = function
    | TInt -> []
    | TNever -> []
    | TStr -> []
    | TBool -> []
    | Poly _ -> []
    | TFun (t1, t2) -> collect_tags_of_unknown t1 @ collect_tags_of_unknown t2
    | TTuple ts -> List.concat @@ List.map collect_tags_of_unknown ts
    | TVariant (ts, _) -> List.concat @@ List.map collect_tags_of_unknown ts
    | TyVar tag -> begin match !store.(tag) with
        | Unknown (_, tag, _) -> [tag]
        | Just (t, _) -> collect_tags_of_unknown t
    end

let rec collect_tags_of_poly = function
    | TInt -> []
    | TNever -> []
    | TStr -> []
    | TBool -> []
    | Poly tag -> [tag]
    | TFun (t1, t2) -> collect_tags_of_poly t1 @ collect_tags_of_poly t2
    | TTuple ts -> List.concat @@ List.map collect_tags_of_poly ts
    | TVariant (ts, _) -> List.concat @@ List.map collect_tags_of_poly ts
    | TyVar _ -> []

let rec occur_check tag t2 =
    let unknowns = collect_tags_of_unknown t2 in
    if List.for_all ((<>) tag) unknowns
    then ()
    else raise CyclicType

let rec unify t1 t2 =
    match t1, t2 with
    | TyVar v1, TyVar v2 -> begin match !store.(v1), !store.(v2) with
        (* UnknownとUnknownの場合はレベルが低い方に合わせる *)
        | Unknown (level1, tag1, l1), Unknown (level2, tag2, l2) when level1 < level2 ->
                let l = l1 @ l2 in
                List.map (fun i -> !store.(i) <- Unknown (level1, tag1, l)) l |> ignore
        | Unknown (level1, tag1, l1), Unknown (level2, tag2, l2) ->
                let l = l1 @ l2 in
                List.map (fun i -> !store.(i) <- Unknown (level2, tag2, l)) l |> ignore
        (* Justは中身の型と同一視して良い *)
        | Unknown (_, tag, l1), Just (ty, l2) ->
            occur_check tag ty;
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Just (ty, l)) l |> ignore
        | Just (ty, l1), Unknown (_, tag, l2) ->
            occur_check tag ty;
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Just (ty, l)) l |> ignore
        | Just (ty1, l1), Just (ty2, l2) ->
            unify ty1 ty2;
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Just (ty1, l)) l |> ignore
    end
    (* JustとUnknownのunifyとほぼ同じ *)
    | TyVar v, ty -> begin match !store.(v) with
        | Unknown (_, tag, l) ->
            occur_check tag ty;
            List.map (fun i -> !store.(i) <- Just (ty, l)) l |> ignore
        | Just (ty', l) -> unify ty ty'
    end
    | ty, TyVar v -> begin match !store.(v) with
        | Unknown (_, tag, l) ->
            occur_check tag ty;
            List.map (fun i -> !store.(i) <- Just (ty, l)) l |> ignore
        | Just (ty', l) -> unify ty ty'
    end
    | TInt, TInt -> ()
    | TBool, TBool -> ()
    | TStr, TStr -> ()
    | Poly _, _ -> failwith "uninstantiate poly type"
    |  _, Poly _ -> failwith "uninstantiate poly type"
    | TTuple ts, TTuple ts' ->
        Util.zip ts ts'
        |> List.map (fun (t1, t2) -> unify t1 t2)
        |> ignore
    | TFun (arg1, ret1), TFun (arg2, ret2) ->
        unify arg1 arg2;
        unify ret1 ret2
    | TVariant (tys1, id1), TVariant (tys2, id2) ->
        if id1 <> id2
        then raise UnifyError
        else
            Util.zip tys1 tys2
            |> List.map (fun (t1, t2) -> unify t1 t2)
            |> ignore
    | _, _ -> raise UnifyError


(* レベルベースの型推論[1]の簡単な概略を述べる。
 * ナイーブに新しい変数に不明な型を割り付けてunifyしていくと本来多相性を持つ型であっても単相に推論されうる。
 * そのため何らかの方法で多相な部分を見極め、そこを多相型に変換し、unifyに使う際は多相型から不明な型に再び変換する操作が必要となる。
 * 不明な型を多相型に変換するのがgeneralize（一般化）であり、逆がinstantiate（和訳知らぬ）である。
 * ここではletをキーとして操作するlevelを使ってgeneralizeを行う。
 * letの定義に入る度にlevelを1つインクリメントし、抜ける度に1つデクリメントして
 * そのデクリメントしたレベルより高いレベルを持つ不明な変数を全て一般化する。（レベルの低い不明な型はそのまま）
 * そしてinstantiateではその時のlevelで全ての多相型を不明な型に変換する。
 * 不明な型と不明な型のunifyの際はレベルが低い方に合わせ単相に寄せる。
 * [1] Rémy, Didier. Extension of ML Type System with a Sorted Equation Theory on Types. 1992.
 *)

let rec inst_ty env =
    let (level, tbl) = env in
    function
    | TInt -> TInt
    | TBool -> TBool
    | TNever -> TNever
    | TStr -> TStr
    | TFun (f, arg) -> TFun (inst_ty env f, inst_ty env arg)
    | TTuple tys -> TTuple (List.map (inst_ty env) tys)
    (* テーブルを見て未登場なら不明な型を割り付ける。
     * 既に型があれば流用する（同じ多相タグには同じ不明な型が割り付けられる必要がある *)
    | Poly tag -> begin match Tbl.lookup_mut tag tbl with
        | Some u -> u
        | None ->
            let u = fresh level in 
            Tbl.push_mut tag u tbl;
            u
    end
    | TyVar i -> TyVar i
    | TVariant (args, id) -> TVariant (List.map (inst_ty env) args, id)

let rec collect_expansive_poly_tags = function
    | Int _ -> []
    | Bool _ -> []
    | Never -> []
    | Var _ -> []
    | Fun _ -> []
    | App ((_, f_ty), (_, arg_ty), _) -> collect_tags_of_unknown f_ty @ collect_tags_of_unknown arg_ty
    | If (c, t, e, _, _) -> (collect_expansive_poly_tags c) @ (collect_expansive_poly_tags t) @ (collect_expansive_poly_tags e)
    | CtorApp (_, _, args, _) -> List.concat @@ List.map (fun (arg, _) -> collect_expansive_poly_tags arg) args
    | Tuple (es, _) -> List.concat @@ List.map (fun (e, _) -> collect_expansive_poly_tags e) es
    | Let (defs, (e, _)) -> collect_expansive_poly_tags e @ List.concat @@ List.map (fun (_, _, (e, _)) -> collect_expansive_poly_tags e) defs
    | LetRec (defs, (e, _)) -> collect_expansive_poly_tags e @ List.concat @@ List.map (fun (_, _, (e, _)) -> collect_expansive_poly_tags e) defs
    | Match ((target, _), arms, _) ->
        collect_expansive_poly_tags target
        @ List.concat @@ List.map (fun (_, _, guard, expr) -> collect_expansive_poly_tags guard @ collect_expansive_poly_tags expr) arms

let rec gen_ty deny level =
    function
    | TInt -> TInt
    | TBool -> TBool
    | TNever -> TNever
    | TStr -> TStr
    | TFun (f, arg) -> TFun (gen_ty deny level f, gen_ty deny level arg)
    | TTuple tys -> TTuple (List.map (gen_ty deny level) tys)
    | Poly tag -> Poly tag
    | TyVar i -> begin match !store.(i) with
        (* Justの場合は実質中身の型と同じとみなして良い
         * Unknownでレベルが現在のレベルより高い場合は一般化する。
         * 同じ不明な型には同じtagが付くようunifyしているのでtagをそのまま流用
         * 最後にdereferenceする際に連番にする*)
        | Unknown (level', tag, _) when level' > level && List.for_all ((<>) tag) deny ->
            Poly tag
        | Unknown _ ->
            TyVar i
        | Just (ty, _) -> gen_ty deny level ty
    end
    | TVariant (args, id) -> TVariant (List.map (gen_ty deny level) args, id)

let rec gen_pat deny level = function
    | PInt (i, p) -> PInt (i, p)
    | PBool (b, p) -> PBool (b, p)
    | PVar (id, ty, p) -> PVar (id, gen_ty deny level ty, p)
    | PTuple (pats, p) -> PTuple (List.map (fun (pat, ty) -> gen_pat deny level pat, gen_ty deny level ty) pats, p)
    | As (pats, ty, p) -> As (List.map (gen_pat deny level) pats, gen_ty deny level ty, p)
    | Or (pat, pats, ty, p) -> Or (gen_pat deny level pat, List.map (gen_pat deny level) pats, gen_ty deny level ty, p)
    | PCtorApp (id, args, ty, p) -> PCtorApp (id, List.map (fun (pat, ty) -> gen_pat deny level pat, gen_ty deny level ty) args, gen_ty deny level ty, p)

let rec gen deny level = function
    | Never -> Never
    | Int (i, p) -> Int (i, p)
    | Bool (b, p) -> Bool (b, p)
    | Var (id, ty, p) -> Var (id, gen_ty deny level ty, p)
    | If (c, t, e, ty, p) -> If (gen deny level c, gen deny level t, gen deny level e, gen_ty deny level ty, p)
    | Fun ((arg, arg_ty), (body, body_ty), p) ->
        Fun ((arg, gen_ty deny level arg_ty), (gen deny level body, gen_ty deny level body_ty), p)
    | Tuple (es, p) -> Tuple (List.map (fun (e, ty) -> gen deny level e, gen_ty deny level ty) es, p)
    | App ((f, f_ty), (arg, arg_ty), p) -> App ((gen deny level f, gen_ty deny level f_ty), (gen deny level arg, gen_ty deny level arg_ty), p)
    | CtorApp (id, p, args, ty) ->
        CtorApp (id, p, List.map (fun (arg, arg_ty) -> gen deny level arg, gen_ty deny level arg_ty) args, gen_ty deny level ty)
    | Let (defs, (e, ty)) ->
        Let (
            List.map (fun ((pat, pat_ty), p, (def, def_ty)) -> (gen_pat deny level pat, gen_ty deny level pat_ty), p, (gen deny level def, gen_ty deny level def_ty)) defs,
            (gen deny level e, gen_ty deny level ty))
    | LetRec (defs, (e, ty)) ->
        LetRec(
            List.map (fun (id, p, (def, def_ty)) -> id, p, (gen deny level def, gen_ty deny level def_ty)) defs,
            (gen deny level e, gen_ty deny level ty)
        )
    | Match ((target, target_ty), arms, ty) ->
        Match (
            (gen deny level target, gen_ty deny level target_ty),
            List.map (fun ((pat, pat_ty), p, guard, e) -> ((gen_pat deny level pat, gen_ty deny level pat_ty), p, gen deny level guard, gen deny level e)) arms,
            gen_ty deny level ty
        )

let rec types2typing = function
    | Types.Never -> failwith "Never type only for internal implementation"
    | Types.Bool -> TBool
    | Types.Int -> TInt
    | Types.Str -> TStr
    | Types.Poly tag -> Poly tag
    | Types.Fun (f, arg) -> TFun (types2typing f, types2typing arg)
    | Types.Tuple ts -> TTuple (List.map types2typing ts)
    | Types.Variant (args, id) -> TVariant (List.map types2typing args, id)

let pervasive_env =
    let venv = List.map (fun (id, (_, ty)) -> id, types2typing ty) Pervasives.vars in
    let cenv = List.map (fun (id, (_, args), (_, ty)) -> id, (List.map types2typing args, types2typing ty)) Pervasives.ctors in
    (venv, cenv, Pervasives.types)

let rec f_pat level env = function
    | Ast.PInt (i, p) -> TInt, PInt (i, p), []
    | Ast.PBool (b, p) -> TBool, PBool (b, p), []
    | Ast.PTuple (ts, p) ->
        let tys, pats, penvs = Util.unzip3 @@ List.map (f_pat level env) ts in
        (TTuple tys, PTuple (Util.zip pats tys, p), List.concat penvs)
    | Ast.As (pats, p) ->
        let tys, pats, penvs = Util.unzip3 @@ List.map (f_pat level env) pats in
        let ty = List.hd tys in
        List.map (fun ty' -> unify ty ty') (List.tl tys) |> ignore;
        (ty, As (pats, ty, p), List.concat penvs)
    | Ast.Or (pat, pats, p) ->
        let ty, pat, penv = f_pat level env pat in
        let tys, pats, penvs = Util.unzip3 @@ List.map (f_pat level env) pats in
        List.map (fun ty' -> unify ty ty') tys |> ignore;
        ty, Or (pat, pats, ty, p), penv @ List.concat penvs
    | Ast.PVar (id, p) ->
        let u = fresh level in
        u, PVar (id, u, p), [id, u]
    | Ast.PCtorApp (cid, args, p) ->
        let param_tys, ty = Idtbl.lookup cid env |> Idtbl.expect "internal error" in
        let inst_tbl = ref [] in
        let param_tys = List.map (inst_ty (level, inst_tbl)) param_tys in
        let ty = inst_ty (level, inst_tbl) ty in
        let tys, pats, penvs = Util.unzip3 @@ List.map (f_pat level env) args in
        let arg_check param_tys arg_tys =
            if (List.length param_tys) <> (List.length arg_tys)
            then failwith @@ Printf.sprintf "%s number of arguments is differ (pattern)" (Lex.show_pos_t p)
            else
                ignore @@ List.map (fun (arg_ty, arg_ty') -> unify arg_ty arg_ty') @@ Util.zip arg_tys param_tys
        in
        begin match param_tys, tys with
        (* 現状の仕様ではタプルのリテラルが来た場合は複数引数かタプルとしての単一引数かで整合性がある方に合わせる必要がある
         * Astでタプルリテラルは一旦全て複数引数として解釈しているのでコンストラクタの引数だけ見れば良い*)
        (* コンストラクタがタプルを取る場合
         * 引数の数とタプルの要素数が同じならタプルをその場で生成し適用していると解釈する *)
        | [TTuple param_tys], arg_tys when (List.length param_tys) = (List.length arg_tys) ->
            arg_check param_tys arg_tys;
            ty, PCtorApp (cid, [PTuple (Util.zip pats tys, p), TTuple tys], ty, p), List.concat penvs
        (* それ以外の場合はそのまま *)
        | param_tys, arg_tys ->
            arg_check param_tys arg_tys; 
            ty, PCtorApp (cid, (Util.zip pats tys), ty, p), List.concat penvs
        end

let rec take_polytags = function
    | TBool -> []
    | TInt -> []
    | TNever -> []
    | TStr -> []
    | TFun (f, arg) -> (take_polytags f) @ (take_polytags arg)
    | TTuple ts -> List.concat @@ List.map take_polytags ts
    | Poly tag -> [tag]
    | TVariant (ts, _) -> List.concat @@ List.map take_polytags ts
    | TyVar idx -> begin match !store.(idx) with
        | Unknown _ -> []
        | Just (t, _) -> take_polytags t
    end

exception DereferenceError

let dereference ty =
    let polytags = Util.uniq @@ take_polytags ty in
    let poly_n = List.length polytags in
    let map tag =
        let rec f = function
            | tag' :: _ when tag = tag' -> 0
            | _ :: tags -> 1 + f tags
            | [] -> failwith "internal error"
        in f polytags
    in
    let rec f = function
        | TBool -> Types.Bool
        | TInt -> Types.Int
        | TStr -> Types.Str
        | TNever -> Types.Never
        | TFun (g, arg) -> Types.Fun (f g, f arg)
        | TTuple ts -> Types.Tuple (List.map f ts)
        | Poly tag -> Poly (map tag)
        | TVariant (ts, id) -> Types.Variant (List.map f ts, id)
        | TyVar idx -> begin match !store.(idx) with
            | Unknown _ -> raise DereferenceError
            | Just (ty, _) -> f ty
        end
    in (poly_n, f ty)

type canonicalize_type_defs_input_t = (Id.t * Lex.pos_t * string list * Ast.tydef_t) list
let canonicalize_type_defs tenv (defs: canonicalize_type_defs_input_t) =
    let rec lookup_from_codef id =
        let rec f = function
            | (id', _, _, _) as def :: _ when id = id' -> Some def
            | _ :: remain -> f remain
            | [] -> None
        in
        f defs
    in
    let rec reassoc_tvar arg_env = function
        | Types.Int -> Types.Int
        | Types.Bool -> Types.Str
        | Types.Str -> Types.Str
        | Types.Poly i -> List.nth arg_env i
        | Types.Never -> failwith "unreachable!"
        | Types.Fun (arg, ret) -> Types.Fun (reassoc_tvar arg_env arg, reassoc_tvar arg_env ret)
        | Types.Tuple ts -> Types.Tuple (List.map (reassoc_tvar arg_env) ts)
        | Types.Variant (args, id) -> Types.Variant (List.map (reassoc_tvar arg_env) args, id)
    in
    let rec canonicalize_ty arg_env = function
        | Ast.TInt _ -> Types.Int
        | Ast.TBool _ -> Types.Bool
        | Ast.TString _ -> Types.Str
        | Ast.TVar (arg, p) -> Tbl.lookup arg arg_env |> Idtbl.expect (Printf.sprintf "%s unknown type variable '%s" (Lex.show_pos_t p) arg)
        | Ast.TTuple (tys, _) -> Types.Tuple (List.map (canonicalize_ty arg_env) tys)
        | Ast.TApp (ty_args, tid, _) ->
            let ty_args = List.map (canonicalize_ty arg_env) ty_args in
            begin match Idtbl.lookup tid tenv with
            | Some (polyness, ty) ->
                if polyness = List.length ty_args
                then reassoc_tvar ty_args ty
                else failwith "polyness unmatch"
            | None -> begin match lookup_from_codef tid with
                | Some (_, _, targs', Ast.Alias ty) ->
                    if (List.length targs') = (List.length ty_args)
                    then canonicalize_ty (Util.zip targs' ty_args) ty
                    else failwith "polyness unmatch"
                | Some (_, _, targs', Ast.Variant _) ->
                    if (List.length targs') = (List.length ty_args)
                    then Types.Variant (ty_args, tid)
                    else failwith "polyness unmatch"
                | None -> failwith "internal error"
            end
        end
    and canonicalize_type_def = function
        | id, p, targs, Ast.Alias ty -> (id, ((List.length targs), canonicalize_ty (List.mapi (fun i arg -> (arg, Types.Poly i)) targs) ty)), []
        | id, p, targs, Ast.Variant arms ->
            let arg_env = List.mapi (fun i arg -> (arg, Types.Poly i)) targs in
            let ty = Types.Variant (List.map snd arg_env, id) in
            let ctors = List.map (fun (id, p, tys) -> (id, (List.map (fun ty -> ty |> canonicalize_ty arg_env |> types2typing) tys, types2typing ty))) arms in
            (id, ((List.length targs), ty)), ctors
    in
    let tydefs, ctors = Util.unzip @@ List.map canonicalize_type_def defs in
    tydefs, List.concat ctors

let env_ref = ref ([], [], [])

let rec f level env =
    let venv, cenv, tenv = env in
    function
    | Ast.Never ->
        env_ref := env;
        TNever, Never
    | Ast.Int (i, p) -> TInt, Int (i, p)
    | Ast.Bool (b, p) -> TBool, Bool (b, p)
    | Ast.Var (id, p) ->
        (* 定義が見つからない事はバグ(Alphaでunboundな変数は全て検出されているはず) *)
        let ty = Idtbl.lookup id venv |> Idtbl.expect "internal error" |> inst_ty (level, ref []) in
        ty, Var (id, ty, p)
    | Ast.Fun (arg, body, p) ->
        (* argは変数定義として扱えるが、letと違い多相性は導入されないのでレベル据え置きで不明な型と置く *)
        let u = fresh level in
        let venv = Idtbl.push arg u venv in
        let body_ty, body = f level (venv, cenv, tenv) body in
        TFun(u, body_ty), Fun((arg, u), (body, body_ty), p)
    | Ast.If (cond_e, then_e, else_e, p) ->
        let cond_ty, cond_e = f level env cond_e in
        let then_ty, then_e = f level env then_e in
        let else_ty, else_e = f level env else_e in
        (* bool, 'a, 'a *)
        unify cond_ty TBool;
        unify then_ty else_ty;
        then_ty, If (cond_e, then_e, else_e, then_ty, p)
    | Ast.App (g, arg, p) ->
        let g_ty, g = f level env g in
        let arg_ty, arg = f level env arg in
        (* arg_ty -> 'aとなるはずなのでそのように型付け *)
        unify (TFun (arg_ty, fresh level)) g_ty;
        (* generalizeを待たず直接dereferenceして返る型を取得する *)
        begin match g_ty with
        | TyVar idx -> begin match !store.(idx) with
            | Just (TFun (_, ret_ty), _) -> ret_ty, App ((g, g_ty), (arg, arg_ty), p)
            | _ -> raise TypingError
            end
        | TFun (_, ret_ty) -> ret_ty, App ((g, g_ty), (arg, arg_ty), p)
        | _ -> raise TypingError
        end
    | Ast.Tuple (es, p) ->
        let tys, es = Util.unzip @@ List.map (f level env) es in
        TTuple tys, Tuple (Util.zip es tys, p)
    | Ast.Let (defs, expr) ->
        (* letの右辺に入る際にレベルを1段上げる。letの定義の左辺も右辺と同じものが来るのでレベルを1段上げる *)
        let def_tys, def_exprs = Util.unzip @@ List.map (fun (_, _, def_expr) -> f (1 + level) env def_expr) defs in
        let pat_tys, pats, pvenv = Util.unzip3 @@ List.map (fun (pat, _, _) -> f_pat (1 + level) cenv pat) defs in
        let ps = List.map Util.snd defs in
        (* 左辺のパターンと右辺の値をunifyする *)
        Util.zip pat_tys def_tys |> List.map (fun (pat_ty, def_ty) -> unify pat_ty def_ty) |> ignore;
        (* letを抜けるのでgeneralize *)
        (* denylistはsyntacti valueでない式に含まれるunknownのタグを集めたもの
         * これを使って多相化を阻止してvalue restrictionを行う
         * tagは一意なので纏めて扱って良い *)
        let denylist = List.concat @@ List.map collect_expansive_poly_tags def_exprs in
        let def_tys = List.map (gen_ty denylist level) def_tys in
        let def_exprs = List.map (gen denylist level) def_exprs in
        let pat_tys = List.map (gen_ty denylist level) pat_tys in
        let pats = List.map (gen_pat denylist level) pats in
        (* generalizeしたvenvの追加分を定義 *)
        let venv' = List.map (fun (id, ty) -> id, gen_ty denylist level ty) @@ List.concat pvenv in
        (* exprをletで定義した型を使って型付け *)
        let ty, expr = f level (venv' @ venv, cenv, tenv) expr in
        (* generalize済みの定義を構築 *)
        let pats = Util.zip pats pat_tys in
        let def_exprs = Util.zip def_exprs def_tys in
        let defs = Util.zip3 pats ps def_exprs in
        ty, Let (defs, (expr, ty))
    | Ast.LetRec (defs, expr) ->
        (* 先に左辺のidを登録した環境を作る *)
        let venv' = List.map (fun (id, _, _) -> (id, fresh (1 + level))) defs in
        let ids = List.map Util.fst defs in
        let ps = List.map Util.snd defs in
        let env' = (venv' @ venv, cenv, tenv) in
        let def_tys, def_exprs = Util.unzip @@ List.map (fun (_, _, def_expr) -> f (1 + level) env' def_expr) defs in
        (* 左辺と右辺をunify *)
        Util.zip def_tys (List.map snd venv') |> List.map (fun (id_ty, def_ty) -> unify id_ty def_ty) |> ignore;
        (* letを抜けるのでgeneralize *)
        let denylist = List.concat @@ List.map collect_expansive_poly_tags def_exprs in
        let def_exprs = List.map (gen denylist level) def_exprs in
        let tys = List.map (gen_ty denylist level) def_tys in
        (* 一般化した型付け済みの環境を再構築 *)
        let venv' = Util.zip ids tys in
        (* letに続く式の型付け *)
        let ty, expr = f level (venv' @ venv, cenv, tenv) expr in
        (* ast構築 *)
        let defs = Util.zip def_exprs tys in
        let defs = Util.zip3 ids ps defs in
        ty, LetRec (defs, (expr, ty))
    | Ast.CtorApp (cid, p, args) ->
        (* Ctorの型をtblでインスタンス化 *)
        let inst_tbl = ref [] in
        let param_tys, ty = Idtbl.lookup cid cenv |> Idtbl.expect "internal error" in
        let ty = inst_ty (level, inst_tbl) ty in
        let param_tys = List.map (inst_ty (level, inst_tbl)) param_tys in
        let arg_tys, args = Util.unzip @@ List.map (f level env) args in
        let arg_check arg_tys param_tys =
            if (List.length arg_tys) <> (List.length param_tys)
            then failwith @@ Printf.sprintf "%s number of arguments is differ" (Lex.show_pos_t p)
            else
                ignore @@ List.map (fun (arg_ty, arg_ty') -> unify arg_ty arg_ty') @@ Util.zip param_tys arg_tys
        in
        begin match param_tys, arg_tys with
        (* 現状の仕様ではタプルのリテラルが来た場合は複数引数かタプルとしての単一引数かで整合性がある方に合わせる必要がある
         * Astでタプルリテラルは一旦全て複数引数として解釈しているのでコンストラクタの引数だけ見れば良い*)
        (* コンストラクタがタプルを取る場合
         * 引数の数とタプルの要素数が同じならタプルをその場で生成し適用していると解釈する *)
        | [TTuple param_tys], arg_tys when (List.length arg_tys) = (List.length param_tys) ->
            arg_check arg_tys param_tys;
            ty, CtorApp (cid, p, [Tuple (Util.zip args arg_tys, p), TTuple arg_tys], ty)
        (* それ以外の場合はそのまま *)
        | param_tys, arg_tys ->
            arg_check arg_tys param_tys; 
            ty, CtorApp (cid, p, (Util.zip args arg_tys), ty)
        end
    | Ast.Match (target, arms) ->
        (* マッチ対象の型付け *)
        let target_ty, target = f level env target in
        let pats, ps, guards, exprs = arms |> List.map (fun (pat, p, guard, expr) ->
            (* 先にパターンを型付けし、パターンによって追加される変数を取得 *)
            let pat_ty, pat, penv = f_pat level cenv pat in
            (* パターンによって拡張された環境でexprとguardを型付け *)
            let ty, expr = f level (penv @ venv, cenv, tenv) expr in
            let guard_ty, guard = f level (penv @ venv, cenv, tenv) guard in
            (* guardの型はboolとなる *)
            unify guard_ty TBool;
            (pat, pat_ty), p, guard, (expr, ty)
        ) |> Util.unzip4 in
        let pat_tys = List.map snd pats in
        let tys = List.map snd exprs in
        let exprs = List.map fst exprs in
        (* 全てのアームでパターンの型と右辺の型について単一化 *)
        ignore @@ List.map (fun pat_ty -> unify (List.hd pat_tys) pat_ty) (List.tl pat_tys);
        ignore @@ List.map (fun ty -> unify (List.hd tys) ty) (List.tl tys);
        unify (List.hd pat_tys) target_ty;
        (List.hd tys), Match ((target, target_ty), Util.zip4 pats ps guards exprs, List.hd tys)
    | Ast.Type (defs, expr) ->
        let tydefs, ctors = canonicalize_type_defs tenv @@ (List.map (fun (id, p, targs, tydef) -> (id, p, List.map fst targs, tydef))) defs in
        let env = (venv, ctors @ cenv, tydefs @ tenv) in
        let ty, expr = f level env expr in
        ty, expr

let f = f 0
