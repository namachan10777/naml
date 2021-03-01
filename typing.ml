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

type tydef_t =
    | Variant of (Id.t * Lex.pos_t * Types.t list) list
    | Alias of Types.t
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
    | Match of (t * ty) * ((pat_t * ty) * Lex.pos_t * t * (t * ty)) list
    | App of (t * ty) * (t * ty) * Lex.pos_t
    | Type of (Id.t * Lex.pos_t * (string * Lex.pos_t) list * tydef_t) list * (t * ty)
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

let rec unify t1 t2 = match t1, t2 with
    | TyVar v1, TyVar v2 -> begin match !store.(v1), !store.(v2) with
        (* UnknownとUnknownの場合はレベルが低い方に合わせる *)
        | Unknown (level1, tag, l1), Unknown (level2, _, l2) when level1 < level2 ->
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Unknown (level1, tag, l)) l |> ignore
        | Unknown (level1, _, l1), Unknown (level2, tag, l2) ->
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Unknown (level2, tag, l)) l |> ignore
        (* Justは中身の型と同一視して良い *)
        | Unknown (_, _, l1), Just (ty, l2) ->
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Just (ty, l)) l |> ignore
        | Just (ty, l1), Unknown (_, _, l2) ->
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Just (ty, l)) l |> ignore
        | Just (ty1, l1), Just (ty2, l2) ->
            unify ty1 ty2;
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Just (ty1, l)) l |> ignore
    end
    (* JustとUnknownのunifyとほぼ同じ *)
    | TyVar v, ty -> begin match !store.(v) with
        | Unknown (_, _, l) ->
            List.map (fun i -> !store.(i) <- Just (ty, l)) l |> ignore
        | Just (ty', l) -> unify ty ty'
    end
    | ty, TyVar v -> begin match !store.(v) with
        | Unknown (_, _, l) ->
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

let rec inst_pat env = function
    | PInt (i, p) -> PInt (i, p)
    | PBool (b, p) -> PBool (b, p)
    | PVar (id, ty, p) -> PVar (id, inst_ty env ty, p)
    | PTuple (pats, p) -> PTuple (List.map (fun (pat, ty) -> inst_pat env pat, inst_ty env ty) pats, p)
    | As (pats, ty, p) -> As (List.map (inst_pat env) pats, inst_ty env ty, p)
    | Or (pat, pats, ty, p) -> Or (inst_pat env pat, List.map (inst_pat env) pats, inst_ty env ty, p)
    | PCtorApp (id, args, ty, p) -> PCtorApp (id, List.map (fun (pat, ty) -> inst_pat env pat, inst_ty env ty) args, inst_ty env ty, p)

let rec inst env = function
    | Never -> Never
    | Int (i, p) -> Int (i, p)
    | Bool (b, p) -> Bool (b, p)
    | Var (id, ty, p) -> Var (id, inst_ty env ty, p)
    | If (c, t, e, ty, p) -> If (inst env c, inst env t, inst env e, inst_ty env ty, p)
    | Fun ((arg, arg_ty), (body, body_ty), p) ->
        Fun ((arg, inst_ty env arg_ty), (inst env body, inst_ty env body_ty), p)
    | Tuple (es, p) -> Tuple (List.map (fun (e, ty) -> inst env e, inst_ty env ty) es, p)
    | App ((f, f_ty), (arg, arg_ty), p) -> App ((inst env f, inst_ty env f_ty), (inst env arg, inst_ty env arg_ty), p)
    | CtorApp (id, p, args, ty) ->
        CtorApp (id, p, List.map (fun (arg, arg_ty) -> inst env arg, inst_ty env arg_ty) args, inst_ty env ty)
    | Let (defs, (e, ty)) ->
        Let (
            List.map (fun ((pat, pat_ty), p, (def, def_ty)) -> (inst_pat env pat, inst_ty env pat_ty), p, (inst env def, inst_ty env def_ty)) defs,
            (inst env e, inst_ty env ty)
        )
    | LetRec (defs, (e, ty)) ->
        LetRec(
            List.map (fun (id, p, (def, def_ty)) -> id, p, (inst env def, inst_ty env def_ty)) defs,
            (inst env e, inst_ty env ty)
        )
    | Match ((target, target_ty), arms) ->
        Match (
            (inst env target, inst_ty env target_ty),
            List.map (fun ((pat, pat_ty), p, guard, (e, ty)) -> ((inst_pat env pat, inst_ty env pat_ty), p, inst env guard, (inst env e, inst_ty env ty))) arms
        )
    | Type (defs, (e, ty)) -> Type (defs, (e, ty))

let rec gen_ty level =
    function
    | TInt -> TInt
    | TBool -> TBool
    | TNever -> TNever
    | TStr -> TStr
    | TFun (f, arg) -> TFun (gen_ty level f, gen_ty level arg)
    | TTuple tys -> TTuple (List.map (gen_ty level) tys)
    | Poly tag -> Poly tag
    | TyVar i -> begin match !store.(i) with
        (* Justの場合は実質中身の型と同じとみなして良い
         * Unknownでレベルが現在のレベルより高い場合は一般化する。
         * 同じ不明な型には同じtagが付くようunifyしているのでtagをそのまま流用
         * 最後にdereferenceする際に連番にする*)
        | Unknown (level', tag, _) when level' > level ->
            (Poly tag)
        | Unknown _ -> TyVar i
        | Just (ty, _) -> gen_ty level ty
    end
    | TVariant (args, id) -> TVariant (List.map (gen_ty level) args, id)

let rec gen_pat level = function
    | PInt (i, p) -> PInt (i, p)
    | PBool (b, p) -> PBool (b, p)
    | PVar (id, ty, p) -> PVar (id, gen_ty level ty, p)
    | PTuple (pats, p) -> PTuple (List.map (fun (pat, ty) -> gen_pat level pat, gen_ty level ty) pats, p)
    | As (pats, ty, p) -> As (List.map (gen_pat level) pats, gen_ty level ty, p)
    | Or (pat, pats, ty, p) -> Or (gen_pat level pat, List.map (gen_pat level) pats, gen_ty level ty, p)
    | PCtorApp (id, args, ty, p) -> PCtorApp (id, List.map (fun (pat, ty) -> gen_pat level pat, gen_ty level ty) args, gen_ty level ty, p)

let rec gen level = function
    | Never -> Never
    | Int (i, p) -> Int (i, p)
    | Bool (b, p) -> Bool (b, p)
    | Var (id, ty, p) -> Var (id, gen_ty level ty, p)
    | If (c, t, e, ty, p) -> If (gen level c, gen level t, gen level e, gen_ty level ty, p)
    | Fun ((arg, arg_ty), (body, body_ty), p) ->
        Fun ((arg, gen_ty level arg_ty), (gen level body, gen_ty level body_ty), p)
    | Tuple (es, p) -> Tuple (List.map (fun (e, ty) -> gen level e, gen_ty level ty) es, p)
    | App ((f, f_ty), (arg, arg_ty), p) -> App ((gen level f, gen_ty level f_ty), (gen level arg, gen_ty level arg_ty), p)
    | CtorApp (id, p, args, ty) ->
        CtorApp (id, p, List.map (fun (arg, arg_ty) -> gen level arg, gen_ty level arg_ty) args, gen_ty level ty)
    | Let (defs, (e, ty)) ->
        Let (
            List.map (fun ((pat, pat_ty), p, (def, def_ty)) -> (gen_pat level pat, gen_ty level pat_ty), p, (gen level def, gen_ty level def_ty)) defs,
            (gen level e, gen_ty level ty))
    | LetRec (defs, (e, ty)) ->
        LetRec(
            List.map (fun (id, p, (def, def_ty)) -> id, p, (gen level def, gen_ty level def_ty)) defs,
            (gen level e, gen_ty level ty)
        )
    | Match ((target, target_ty), arms) ->
        Match (
            (gen level target, gen_ty level target_ty),
            List.map (fun ((pat, pat_ty), p, guard, (e, ty)) -> ((gen_pat level pat, gen_ty level pat_ty), p, gen level guard, (gen level e, gen_ty level ty))) arms
        )
    | Type (defs, (e, ty)) -> Type (defs, (e, ty))

let rec types2typing = function
    | Types.Bool -> TBool
    | Types.Int -> TInt
    | Types.Str -> TStr
    | Types.Poly tag -> Poly tag
    | Types.Fun (f, arg) -> TFun (types2typing f, types2typing arg)
    | Types.Tuple ts -> TTuple (List.map types2typing ts)
    | Types.Variant (args, id) -> TVariant (List.map types2typing args, id)

let pervasive_env =
    let venv = List.map (fun (id, (_, ty)) -> id, types2typing ty) Pervasives.vars in
    let cenv = List.map (fun (id, (_, args), (_, ty)) -> id, (List.map types2typing args), types2typing ty) Pervasives.ctors in
    let tenv = List.map (fun (id, (_, ty)) -> id, types2typing ty) Pervasives.types in
    (venv, cenv, tenv)

let rec f_pat level env = function
    | Ast.PInt (i, p) -> TInt, PInt (i, p), []
    | Ast.PVar (id, p) ->
        let u = fresh level in
        u, PVar (id, u, p), [id, u]
    | _ -> failwith "f_pat unimplemented"

let rec f level env =
    let venv, cenv, tenv = env in
    function
    | Ast.Never -> TNever, Never
    | Ast.Int (i, p) -> TInt, Int (i, p)
    | Ast.Bool (b, p) -> TBool, Bool (b, p)
    | Ast.Var (id, p) ->
        (* 定義が見つからない事はバグ(Alphaでunboundな変数は全て検出されているはず) *)
        let ty = Tbl.lookup id venv |> Tbl.expect "internal error" |> inst_ty (level, ref []) in
        ty, Var (id, ty, p)
    | Ast.Fun (arg, body, p) ->
        (* argは変数定義として扱えるが、letと違い多相性は導入されないのでレベル据え置きで不明な型と置く *)
        let u = fresh level in
        let venv = Tbl.push arg u venv in
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
        let def_tys, def_exprs = Util.unzip @@ List.map (fun (_, _, def_expr) -> f (level+1) env def_expr) defs in
        let pat_tys, pats, pvenv = Util.unzip3 @@ List.map (fun (pat, _, _) -> f_pat (level+1) env pat) defs in
        let ps = List.map Util.snd defs in
        (* 左辺のパターンと右辺の値をunifyする *)
        Util.zip pat_tys def_tys |> List.map (fun (pat_ty, def_ty) -> unify pat_ty def_ty) |> ignore;
        (* letを抜けるのでgeneralize *)
        let def_tys = List.map (gen_ty level) def_tys in
        let def_exprs = List.map (gen level) def_exprs in
        let pat_tys = List.map (gen_ty level) pat_tys in
        let pats = List.map (gen_pat level) pats in
        (* generalizeしたvenvの追加分を定義 *)
        let venv' = List.map (fun (id, ty) -> id, gen_ty level ty) @@ List.concat pvenv in
        (* exprをletで定義した型を使って型付け *)
        let ty, expr = f level (venv' @ venv, cenv, tenv) expr in
        (* generalize済みの定義を構築 *)
        let pats = Util.zip pats pat_tys in
        let def_exprs = Util.zip def_exprs def_tys in
        let defs = Util.zip3 pats ps def_exprs in
        ty, Let (defs, (expr, ty))
    | _ -> failwith "unimplemented"
