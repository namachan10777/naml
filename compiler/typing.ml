(* Algorithm W*)

type pat_t =
    | PVar of Types.vid_t * Types.t * Lex.pos_t
    | PTuple of pat_t list * Types.t list * Lex.pos_t
    | PCtor of pat_t list * Types.t list * Types.cid_t * Lex.pos_t
    | PInt of int * Lex.pos_t
    | PBool of bool * Lex.pos_t
    | PStr of string * Lex.pos_t
    | As of pat_t list * Lex.pos_t
    | Or of pat_t * pat_t list * Lex.pos_t
[@@deriving show]

type tydef_t = Abbr of Types.t | Variant of (string * Types.t list) list

exception UnsupportedFunction of string

exception CyclicType

exception Internal

exception UnboundIdentifier of string

exception TypeError of string

type t =
    | Never
    | Int of int * Lex.pos_t
    | Bool of bool * Lex.pos_t
    | Var of Types.vid_t * Lex.pos_t
    | App of t * t list * Lex.pos_t
    | Let of (pat_t * t) list * t
    | LetRec of (Types.vid_t * Types.t * t) list * t
    | If of t * t * t * Lex.pos_t
    | Fun of (Types.vid_t * Types.t) list * t * Types.t * Lex.pos_t
    | Tuple of t list * Types.t list * Lex.pos_t
    | Match of t * Types.t * (pat_t * t) list * Types.t
    | CtorApp of Types.cid_t * t list * Types.t * Lex.pos_t
[@@deriving show]

let rec lookup_v x = function
    | (y, ty) :: remain -> if x = y then ty else lookup_v x remain
    | _ -> raise @@ UnboundIdentifier (Types.show_vid_t x)

let rec lookup_t x = function
    | (y, ty) :: remain -> if x = y then ty else lookup_t x remain
    | _ -> raise @@ UnboundIdentifier (Types.show_tid_t x)

let rec lookup_c x = function
    | (y, ty) :: remain -> if x = y then ty else lookup_c x remain
    | _ -> raise @@ UnboundIdentifier (Types.show_cid_t x)

let count_tag = ref 0

let init () = count_tag := 99

let fresh level =
    count_tag := !count_tag + 1 ;
    Types.Var (ref @@ ref @@ Types.Unknown (level, !count_tag, []))

let rec instantiate env level =
    let lookup i =
        let rec f = function
            | (x, y) :: remain -> if i = x then y else f remain
            | [] ->
                let unk = fresh level in
                env := (i, unk) :: !env ;
                unk
        in
        f !env
    in
    let rec f = function
        | Types.Int -> Types.Int
        | Types.Bool -> Types.Bool
        | Types.Str -> Types.Str
        | Types.Fun (args, ret) -> Types.Fun (List.map f args, f ret)
        | Types.Var _ as t -> t
        | Types.Poly i -> lookup i
        | Types.Tuple ts -> Types.Tuple (List.map f ts)
        | Types.Variant (targs, name) -> Types.Variant (List.map f targs, name)
    in
    f

let rec occur_check id =
    let rec f = function
        | Types.Int -> ()
        | Types.Bool -> ()
        | Types.Str -> ()
        | Types.Poly _ -> ()
        | Types.Tuple ts -> List.map f ts |> ignore
        | Types.Variant (targs, _) -> List.map f targs |> ignore
        | Types.Fun (args, ret) ->
            List.map f args |> ignore ;
            f ret
        | Types.Var u -> (
          match !(!u) with
          | Types.Unknown (_, id', _) ->
              if id = id' then raise CyclicType else ()
          | Types.Just (t, _) -> f t )
    in
    f

let rec deref_ty = function
    | Types.Var u as t -> (
      match !(!u) with Types.Just (t, _) -> deref_ty t | _ -> t )
    | Types.Fun (args, (Types.Fun _ as r)) -> (
      match deref_ty r with
      | Types.Fun (args', r) ->
          Types.Fun (List.map deref_ty (args @ args'), deref_ty r)
      | _ -> raise Internal )
    | Types.Fun (args, ret) -> Types.Fun (List.map deref_ty args, deref_ty ret)
    | Types.Int -> Types.Int
    | Types.Bool -> Types.Bool
    | Types.Str -> Types.Str
    | Types.Tuple ts -> Types.Tuple (List.map deref_ty ts)
    | Types.Poly i -> Types.Poly i
    | Types.Variant (ts, name) -> Types.Variant (List.map deref_ty ts, name)

let rec unify a b =
    let unify_u_and_t u t =
        match !u with
        | Types.Unknown (level, tag, refs) ->
            occur_check tag t ;
            let refs = u :: refs in
            let u = Types.Just (t, refs) in
            List.map (fun r -> r := u) refs |> ignore
        | Types.Just (t', refs) ->
            let refs = u :: refs in
            unify t t' ;
            let u = Types.Just (t, refs) in
            List.map (fun r -> r := u) refs |> ignore
    in
    match (a, b) with
    | Types.Int, Types.Int -> ()
    | Types.Bool, Types.Bool -> ()
    | Types.Fun ([], _), Types.Fun ([], _) -> raise Internal
    | Types.Fun ([], r), b -> unify r b
    | a, Types.Fun ([], r) -> unify r a
    | Types.Fun ([a1], r1), Types.Fun ([a2], r2) -> unify a1 a2 ; unify r1 r2
    | Types.Fun (a1 :: as1, r1), Types.Fun (a2 :: as2, r2) ->
        unify (Types.Fun (as1, r1)) (Types.Fun (as2, r2)) ;
        unify a1 a2
    | Types.Variant (targs, name), Types.Variant (targs', name') ->
        if name = name' then
          List.map (fun (t, t') -> unify t t') (Util.zip targs targs') |> ignore
        else raise @@ TypeError "cannot unify variant type"
    | Types.Tuple ts1, Types.Tuple ts2 ->
        Util.zip ts1 ts2 |> List.map (fun (e1, e2) -> unify e1 e2) |> ignore
    | Types.Var u1, Types.Var u2 -> (
      match (!(!u1), !(!u2)) with
      | Types.Unknown (level, tag, refs1), Types.Unknown (level', _, refs2)
        when level < level' ->
          let refs = (!u1 :: !u2 :: refs1) @ refs2 in
          !u1 := Types.Unknown (level, tag, refs) ;
          List.map (fun r -> r := !(!u1)) refs |> ignore
      | Types.Unknown (_, _, refs1), Types.Unknown (level, tag, refs2) ->
          let refs = (!u1 :: !u2 :: refs1) @ refs2 in
          !u1 := Types.Unknown (level, tag, refs) ;
          List.map (fun r -> r := !(!u1)) refs |> ignore
      | Types.Just (t, refs1), Types.Just (t', refs2) ->
          let refs = (!u1 :: !u2 :: refs1) @ refs2 in
          unify t t' ;
          !u1 := Types.Just (deref_ty t, refs) ;
          List.map (fun r -> r := !(!u1)) refs |> ignore
      | Types.Unknown (_, _, refs1), Types.Just (t, refs2) ->
          let refs = (!u1 :: !u2 :: refs1) @ refs2 in
          !u1 := Types.Just (t, refs) ;
          List.map (fun r -> r := !(!u1)) refs |> ignore
      | Types.Just (t, refs1), Types.Unknown (_, _, refs2) ->
          let refs = (!u1 :: !u2 :: refs1) @ refs2 in
          !u1 := Types.Just (t, refs) ;
          List.map (fun r -> r := !(!u1)) refs |> ignore )
    | Types.Var u, t -> unify_u_and_t !u t
    | t, Types.Var u -> unify_u_and_t !u t
    | a, b ->
        raise
        @@ TypeError
             (Printf.sprintf "cannot unify %s, %s" (Types.show a)
                (Types.show b))

type poly_map_t = Unknown of int | Poly of Types.pid_t

let readable global tag =
    let rec f tbl =
        match (tag, tbl) with
        | Poly tag, (Poly tag', y) :: remain when tag = tag' -> y
        | Unknown tag, (Unknown tag', y) :: remain when tag = tag' -> y
        | _, _ :: remain -> f remain
        | _, [] ->
            global := (tag, Types.Pid (List.length !global)) :: !global ;
            snd @@ List.hd !global
    in
    let x = f !global in
    x

let generalize_ty tbl level =
    let rec f = function
        | Types.Int -> Types.Int
        | Types.Bool -> Types.Bool
        | Types.Str -> Types.Str
        | Types.Variant (targs, name) -> Types.Variant (List.map f targs, name)
        | Types.Var u as ty -> (
          match !(!u) with
          | Types.Just (ty, _) -> f ty
          | Types.Unknown (level', tag, _) ->
              if level' > level then Types.Poly (readable tbl (Unknown tag))
              else ty )
        | Types.Fun (args, ret_ty) -> Types.Fun (List.map f args, f ret_ty)
        | Types.Tuple ts -> Types.Tuple (List.map f ts)
        | Types.Poly tag -> Types.Poly (readable tbl (Poly tag))
    in
    f

let rec generalize_pat tbl level =
    let rec f = function
        | PVar (id, ty, p) -> PVar (id, generalize_ty tbl level ty, p)
        | PTuple (ps, ty, p) ->
            PTuple (List.map f ps, List.map (generalize_ty tbl level) ty, p)
        | PCtor (ps, targs, name, p) ->
            PCtor (List.map f ps, List.map (generalize_ty tbl level) targs, name, p)
        | PInt (i, p) -> PInt (i, p)
        | PBool (b, p) -> PBool (b, p)
        | PStr (s, p) -> PStr (s, p)
        | As (ps, p) -> As (List.map f ps, p)
        | Or (pat, pats, p) -> Or (f pat, List.map f pats, p)
    in
    f

let generalize tbl level =
    let rec f = function
        | Int (i, p) -> Int (i, p)
        | Bool (b, p) -> Bool (b, p)
        | Var (id, p) -> Var (id, p)
        | App (g, args, p) -> App (f g, List.map f args, p)
        | Fun (args, body, ret_ty, p) ->
            Fun
              ( List.map
                  (fun (arg, ty) -> (arg, generalize_ty tbl level ty))
                  args
              , f body
              , generalize_ty tbl level ret_ty
              , p)
        | Tuple (vals, types, p) ->
            Tuple (List.map f vals, List.map (generalize_ty tbl level) types, p)
        | Let (defs, expr) ->
            Let
              ( List.map
                  (fun (pat, def) -> (generalize_pat tbl level pat, f def))
                  defs
              , f expr )
        | LetRec (defs, expr) ->
            LetRec
              ( List.map
                  (fun (name, ty, def) ->
                    (name, generalize_ty tbl level ty, f def))
                  defs
              , f expr )
        | If (cond, e1, e2, p) -> If (f cond, f e1, f e2, p)
        | Match (target, target_ty, arms, ty) ->
            Match
              ( f target
              , generalize_ty tbl level target_ty
              , List.map
                  (fun (pat, expr) -> (generalize_pat tbl level pat, f expr))
                  arms
              , generalize_ty tbl level ty)
        | CtorApp (name, e, t, p) ->
            CtorApp (name, List.map f e, generalize_ty tbl level t, p)
        | Never -> Never
    in
    f

(* TODO: 同じ名前が無いかチェック *)
let pat_ty cenv level =
    let rec lookup_ctor name = function
        | (name', v) :: _ when name = name' -> v
        | _ :: remain -> lookup_ctor name remain
        | [] ->
            raise
            @@ UnboundIdentifier ("Unbound constructor " ^ Types.show_cid_t name)
    in
    let tbl = ref [] in
    let rec ty_for_id id =
        let rec f = function
            | (id', ty) :: _ when id = id' -> ty
            | _ :: remain -> f remain
            | [] ->
                tbl := (id, fresh level) :: !tbl ;
                snd @@ List.hd !tbl
        in
        f !tbl
    in
    let rec f = function
        | Alpha.PVar (name, p) ->
            let ty = ty_for_id name in
            ([(name, ty)], ty, PVar (name, ty, p))
        | Alpha.Or (pat, pats, p) ->
            let names, ty, pat = f pat in
            let _, tys, pats = Util.unzip3 @@ List.map f pats in
            List.map (fun t' -> unify ty t') tys |> ignore ;
            (names, ty, Or (pat, pats, p))
        | Alpha.PTuple (ts, p) ->
            let names, tys, ps = Util.unzip3 @@ List.map f ts in
            (List.concat names, Types.Tuple tys, PTuple (ps, tys, p))
        | Alpha.PInt (i, p) -> ([], Types.Int, PInt (i, p))
        | Alpha.PBool (b, p) -> ([], Types.Bool, PBool (b, p))
        | Alpha.As (ps, p) ->
            let names, tys, ps = Util.unzip3 @@ List.map f ps in
            let t = fresh level in
            List.map (fun t' -> unify t t') tys |> ignore ;
            (List.concat names, deref_ty t, As (ps, p))
        | Alpha.PCtor (cname, args, p) -> (
            let names, tys, args = Util.unzip3 @@ List.map f args in
            match lookup_ctor cname cenv with
            | ([Types.Tuple ts'] | ts'), targs, tname ->
                let tbl = ref [] in
                let ts' = List.map (instantiate tbl level) ts' in
                let targs = List.map (instantiate tbl level) targs in
                if List.length ts' <> List.length tys then
                  raise
                  @@ TypeError
                       (Printf.sprintf
                          "The constructor take %d values. But %d arguments \
                           passed"
                          (List.length tys) (List.length ts'))
                else
                  List.map (fun (t, t') -> unify t t') @@ Util.zip ts' tys
                  |> ignore ;
                let variant_ty =
                    Types.Variant (List.map deref_ty targs, tname)
                in
                ( List.concat names
                , variant_ty
                , PCtor (args, List.map deref_ty targs, cname, p) ) )
    in
    f

let rec canonical_type_def tenv co_def (name, targs, def) =
    (* envはtarg -> Types.tの写像*)
    (* Alpha.typdef -> tydef_t *)
    let rec f_tydef hist (name, targs, tydef) =
        if List.for_all (fun n -> n <> name) hist then
          match tydef with
          | Alpha.Alias ty -> f_ty (name :: hist) targs ty
          | Alpha.Variant ctors -> raise Internal
        else raise CyclicType
    (* Alpha.ty_t -> Types.t *)
    and f_ty hist env = function
        | Alpha.TVar (id, p) -> List.nth env id
        | Alpha.TApp (tys, name, p) -> (
          match lookup_from_co_def name co_def with
          | Some (targs, Alpha.Variant _) ->
              (* TODO 型引数の長さを チェック *)
              Types.Variant (List.map (f_ty hist env) tys, name)
          | Some (targs, Alpha.Alias alias) ->
              let targs = List.map (fun ty -> f_ty hist env ty) tys in
              f_tydef hist (name, targs, Alpha.Alias alias)
          | None ->
              let higher = lookup_from_tenv name tenv in
              (* TODO 型引数の長さを チェック *)
              reassoc_ty
                (List.mapi (fun i ty -> (Types.Pid i, f_ty hist env ty)) tys)
                higher )
        | Alpha.TInt _ -> Types.Int
        | Alpha.TBool _ -> Types.Bool
        | Alpha.TString _ -> Types.Str
        | Alpha.TTuple (ts, _) -> Types.Tuple (List.map (f_ty hist env) ts)
    (* AbbreviationについてPolyを呼び出し元の型で置換 *)
    (* ここのenvはint -> Types.t *)
    and reassoc_ty env =
        let rec map i = function
            | (i', ty) :: _ when i = i' -> ty
            | _ :: remain -> map i remain
            | [] -> raise Internal
        in
        function
        | Types.Str -> Types.Str
        | Types.Int -> Types.Int
        | Types.Bool -> Types.Bool
        | Types.Poly i -> map i env
        | Types.Tuple ts -> Types.Tuple (List.map (reassoc_ty env) ts)
        | Types.Fun (args, ret) ->
            Types.Fun (List.map (reassoc_ty env) args, reassoc_ty env ret)
        | Types.Var _ -> raise Internal
        | Types.Variant (args, name) ->
            Types.Variant (List.map (reassoc_ty env) args, name)
    and lookup_from_tenv id = function
        | (id', tydef) :: _ when id = id' -> tydef
        | _ :: remain -> lookup_from_tenv id remain
        | [] ->
            raise @@ UnboundIdentifier ("undefined type" ^ Types.show_tid_t id)
    and lookup_from_co_def id = function
        | (id', targs, ty) :: _ when id = id' -> Some (targs, ty)
        | _ :: remain -> lookup_from_co_def id remain
        | [] -> None
    in
    (* 先頭の型引数から順にPolyのIdを振る *)
    let targs = List.init targs (fun i -> Types.Poly (Types.Pid i)) in
    match def with
    | Alpha.Alias ty -> ([], f_tydef [] (name, targs, Alpha.Alias ty))
    | Alpha.Variant ctors ->
        let ctors =
            List.map
              (fun (ctor_name, _, args) ->
                (ctor_name, (List.map (f_ty [] targs) args, targs, name)))
              ctors
        in
        (ctors, Types.Variant (targs, name))

(* TODO: confirm one variable only bound once *)
let rec g env level =
    let venv, tenv, cenv = env in
    function
    | Alpha.Int (i, p) -> (Int (i, p), Types.Int)
    | Alpha.Bool (b, p) -> (Bool (b, p), Types.Bool)
    | Alpha.Var (id, p) -> (Var (id, p), lookup_v id venv)
    | Alpha.App (f, args, p) -> (
        let args, arg_tys = Util.unzip @@ List.map (g env level) args in
        let arg_tys = List.map (instantiate (ref []) level) arg_tys in
        let f, f_ty = g env level f in
        let f_ty' = Types.Fun (arg_tys, fresh level) in
        let f_ty = instantiate (ref []) level f_ty in
        unify f_ty f_ty' ;
        match deref_ty f_ty with
        | Types.Fun (params, ret_ty) ->
            if List.length params > List.length args then
              let param_tys = Util.drop (List.length arg_tys) params in
              (App (f, args, p), Types.Fun (param_tys, ret_ty))
            else if List.length params = List.length args then
              (App (f, args, p), ret_ty)
            else
              raise
              @@ TypeError
                   ( Printf.sprintf "too much argument %s"
                   @@ Types.show @@ deref_ty f_ty )
        | _ -> raise @@ TypeError "unmatched app" )
    | Alpha.Let (defs, expr) ->
        let pat_vars, defs =
            Util.unzip
            @@ List.map
                 (fun (pat, def) ->
                   let def, def_ty = g env (level + 1) def in
                   let pat_vars, pat_ty, pat = pat_ty cenv (level + 1) pat in
                   unify pat_ty def_ty ;
                   let def_ty = deref_ty def_ty in
                   let tbl = ref [] in
                   let def, def_ty =
                       (generalize tbl level def, generalize_ty tbl level def_ty)
                   in
                   let pat, def_ty =
                       ( generalize_pat tbl level pat
                       , generalize_ty tbl level pat_ty )
                   in
                   let pat_vars =
                       List.map
                         (fun (name, ty) -> (name, generalize_ty tbl level ty))
                         pat_vars
                   in
                   (pat_vars, (pat, def)))
                 defs
        in
        let expr, ty = g (List.concat pat_vars @ venv, tenv, cenv) level expr in
        (Let (defs, expr), ty)
    | Alpha.LetRec (defs, expr) ->
        let vars = List.map (fun (name, p, _) -> (name, fresh (level + 1))) defs in
        let defs = List.map (fun (_, _, t) -> t) defs in
        let env = (vars @ venv, tenv, cenv) in
        let tbl = ref [] in
        let defs =
            Util.zip vars defs
            |> List.map (fun ((name, ty), def) ->
                   let def, def_ty = g env (level + 1) def in
                   unify ty def_ty ;
                   (name, deref_ty ty, def))
        in
        let defs =
            List.map
              (fun (name, ty, def) ->
                (name, generalize_ty tbl level ty, generalize tbl level def))
              defs
        in
        let expr, ty = g env level expr in
        (LetRec (defs, expr), ty)
    | Alpha.Fun (args, body, p) ->
        let args = List.map (fun arg -> (arg, fresh level)) args in
        let arg_as_vars = List.map (fun (arg, ty) -> (arg, ty)) args in
        let body, body_ty = g (arg_as_vars @ venv, tenv, cenv) level body in
        (Fun (args, body, body_ty, p), Types.Fun (List.map snd args, body_ty))
    | Alpha.Tuple (tp, p) ->
        let elems, types = Util.unzip @@ List.map (g env level) tp in
        (Tuple (elems, types, p), Types.Tuple types)
    | Alpha.If (cond, e1, e2, p) ->
        let cond, cond_ty = g env level cond in
        unify cond_ty Types.Bool ;
        let e1, e1_ty = g env level e1 in
        let e2, e2_ty = g env level e2 in
        unify e1_ty e2_ty ;
        (If (cond, e1, e2, p), deref_ty e1_ty)
    | Alpha.Never -> (Never, Types.Tuple [])
    | Alpha.Ctor (name, p) -> (
      match lookup_c name cenv with
      | [], targs, variant_name ->
          let ty =
              instantiate (ref []) level (Types.Variant (targs, variant_name))
          in
          (CtorApp (name, [], ty, p), ty)
      | _ ->
          raise
          @@ TypeError
               ("Ctor " ^ Types.show_cid_t name ^ " takes some arguments") )
    | Alpha.CtorApp (name, p, args) -> (
      match lookup_c name cenv with
      | [Types.Tuple arg_tys'], targs, variant_name ->
          let args, arg_tys = Util.unzip @@ List.map (g env level) args in
          let tbl = ref [] in
          let arg_tys = List.map (instantiate tbl level) arg_tys in
          let tbl = ref [] in
          let arg_tys' = List.map (instantiate tbl level) arg_tys' in
          let variant_ty =
              instantiate tbl level @@ Types.Variant (targs, variant_name)
          in
          Util.zip arg_tys arg_tys'
          |> List.map (fun (arg_ty, arg_ty') -> unify arg_ty arg_ty')
          |> ignore ;
          (CtorApp (name, [Tuple (args, arg_tys, p)], variant_ty, p), variant_ty)
      | arg_tys', targs, variant_name ->
          let args, arg_tys = Util.unzip @@ List.map (g env level) args in
          let tbl = ref [] in
          let arg_tys = List.map (instantiate tbl level) arg_tys in
          let tbl = ref [] in
          let arg_tys' = List.map (instantiate tbl level) arg_tys' in
          let variant_ty =
              instantiate tbl level @@ Types.Variant (targs, variant_name)
          in
          Util.zip arg_tys arg_tys'
          |> List.map (fun (arg_ty, arg_ty') -> unify arg_ty arg_ty')
          |> ignore ;
          (CtorApp (name, args, variant_ty, p), variant_ty) )
    | Alpha.Match (target, arms) ->
        let target, target_ty = g env level target in
        let pats, guards, exprs =
            Util.unzip3
            @@ List.map
                 (fun (pat, guard, expr) ->
                   let tbl = ref [] in
                   let pat_vars, pat_ty, pat = pat_ty cenv level pat in
                   let pat_vars =
                       List.map
                         (fun (name, ty) -> (name, instantiate tbl level ty))
                         pat_vars
                   in
                   let pat_ty = instantiate tbl level pat_ty in
                   let venv = pat_vars @ venv in
                   let env = (venv, tenv, cenv) in
                   let expr, expr_ty = g env level expr in
                   let guard, guard_ty = g env level guard in
                   ((pat_vars, pat, pat_ty), (guard, guard_ty), (expr, expr_ty)))
                 arms
        in
        let pat_vars, pats, pat_tys = Util.unzip3 pats in
        let guards, guard_tys = Util.unzip guards in
        let exprs, expr_tys = Util.unzip exprs in
        let target_ty =
            List.fold_left
              (fun t t' -> unify t t' ; deref_ty t)
              target_ty pat_tys
        in
        let t =
            List.fold_left
              (fun t t' -> unify t t' ; deref_ty t)
              (fresh level) expr_tys
        in
        List.map (fun t -> unify t Types.Bool) guard_tys |> ignore ;
        (Match (target, target_ty, Util.zip pats exprs, t), t)
    | Alpha.Type (defs, expr) ->
        let names = List.map (fun (name, _, _, _) -> name) defs in
        let defs =
            List.map (fun (name, _, targs, tydef) -> (name, targs, tydef)) defs
        in
        let defs = List.map (canonical_type_def tenv defs) defs in
        let ctors, tydefs = Util.unzip defs in
        let tydefs = Util.zip names tydefs in
        let ctors = List.concat ctors in
        let env = (venv, tydefs @ tenv, ctors @ cenv) in
        g env level expr

let pervasive_tenv = List.map (fun (_, i, t) -> (i, t)) Types.pervasive_types

let pervasive_venv = List.map (fun (_, i, t) -> (i, t)) Types.pervasive_vals

let pervasive_cenv = List.map (fun (_, i, t) -> (i, t)) Types.pervasive_ctors

let f ast = fst @@ g (pervasive_venv, pervasive_tenv, pervasive_cenv) 0 ast
