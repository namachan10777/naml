(* Algorithm W*)

type id_t = Ast.id_t [@@deriving show]

type pat_t = PVar of string * Types.t | PTuple of pat_t list * Types.t list
[@@deriving show]

type tydef_t = Abbr of Types.t | Variant of (string * Types.t list) list

type t =
    | Never
    | Int of int
    | Bool of bool
    | Var of id_t
    | App of t * t list
    | Let of (pat_t * t) list * t
    | LetRec of (string list * Types.t * t) list * t
    | If of t * t * t
    | Fun of (string * Types.t) list * t * Types.t
    | Tuple of t list * Types.t list
    | Match of t * Types.t * (pat_t * t) list * Types.t
    | Ctor of string list * Types.t
    | CtorApp of string list * t list * Types.t
[@@deriving show]

let rec lookup x = function
    | (y, ty) :: remain -> if x = y then ty else lookup x remain
    | _ -> failwith @@ Printf.sprintf "notfound %s" @@ Ast.show_id_t x

let count = ref 0

let init () = count := 99

let fresh level =
    count := !count + 1 ;
    Types.Var (ref @@ ref @@ Types.Unknown (level, !count, []))

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
        | Types.Fun (args, ret) -> Types.Fun (List.map f args, f ret)
        | Types.Var _ as t -> t
        | Types.Poly i -> lookup i
        | Types.Tuple ts -> Types.Tuple (List.map f ts)
        | Types.Variant (targs, name) -> Types.Variant (List.map f targs, name)
        | _ -> failwith "instantiate unimplemented"
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
              if id = id' then failwith "cyclic type" else ()
          | Types.Just (t, _) -> f t )
    in
    f

let rec unify a b =
    let unify_u_and_t u t =
        match !u with
        | Types.Unknown (level, tag, refs) ->
            occur_check tag t ;
            let refs = u :: refs in
            let u = Types.Just (t, refs) in
            List.map (fun r -> r := u) refs |> ignore ;
            t
        | Types.Just (t', refs) ->
            let refs = u :: refs in
            let t = unify t t' in
            let u = Types.Just (t, refs) in
            List.map (fun r -> r := u) refs |> ignore ;
            t
    in
    match (a, b) with
    | Types.Int, Types.Int -> Types.Int
    | Types.Bool, Types.Bool -> Types.Bool
    | Types.Fun ([], _), Types.Fun ([], _) -> failwith "unreachable"
    | Types.Fun ([], r), b -> unify r b
    | a, Types.Fun ([], r) -> unify r a
    | Types.Fun ([a1], r1), Types.Fun ([a2], r2) ->
        Types.Fun ([unify a1 a2], unify r1 r2)
    | Types.Fun (a1 :: as1, r1), Types.Fun (a2 :: as2, r2) -> (
      match unify (Types.Fun (as1, r1)) (Types.Fun (as2, r2)) with
      | Types.Fun (as', r) -> Types.Fun (unify a1 a2 :: as', r)
      | _ -> failwith "cannot unify fun" )
    | Types.Variant (targs, name), Types.Variant (targs', name') ->
        if name = name' then
          Types.Variant
            (List.map (fun (t, t') -> unify t t') (Util.zip targs targs'), name)
        else failwith @@ "cannot unify higher type"
    | Types.Tuple ts1, Types.Tuple ts2 ->
        Types.Tuple (Util.zip ts1 ts2 |> List.map (fun (e1, e2) -> unify e1 e2))
    | Types.Var u1, Types.Var u2 -> (
      match (!(!u1), !(!u2)) with
      | Types.Unknown (level, tag, refs1), Types.Unknown (level', _, refs2)
        when level < level' ->
          let refs = (!u1 :: !u2 :: refs1) @ refs2 in
          !u1 := Types.Unknown (level, tag, refs) ;
          List.map (fun r -> r := !(!u1)) refs |> ignore ;
          Types.Var u1
      | Types.Unknown (_, _, refs1), Types.Unknown (level, tag, refs2) ->
          let refs = (!u1 :: !u2 :: refs1) @ refs2 in
          !u1 := Types.Unknown (level, tag, refs) ;
          List.map (fun r -> r := !(!u1)) refs |> ignore ;
          Types.Var u1
      | Types.Just (t, refs1), Types.Just (t', refs2) ->
          let refs = (!u1 :: !u2 :: refs1) @ refs2 in
          !u1 := Types.Just (unify t t', refs) ;
          List.map (fun r -> r := !(!u1)) refs |> ignore ;
          Types.Var u1
      | Types.Unknown (_, _, refs1), Types.Just (t, refs2) ->
          let refs = (!u1 :: !u2 :: refs1) @ refs2 in
          !u1 := Types.Just (t, refs) ;
          List.map (fun r -> r := !(!u1)) refs |> ignore ;
          Types.Var u1
      | Types.Just (t, refs1), Types.Unknown (_, _, refs2) ->
          let refs = (!u1 :: !u2 :: refs1) @ refs2 in
          !u1 := Types.Just (t, refs) ;
          List.map (fun r -> r := !(!u1)) refs |> ignore ;
          Types.Var u1 )
    | Types.Var u, t -> unify_u_and_t !u t
    | t, Types.Var u -> unify_u_and_t !u t
    | a, b ->
        failwith
        @@ Printf.sprintf "cannot unify %s, %s" (Types.show a) (Types.show b)

type poly_map_t = Unknown of int | Poly of int

let readable global tag =
    let rec f tbl =
        match (tag, tbl) with
        | Poly tag, (Poly tag', y) :: remain when tag = tag' -> y
        | Unknown tag, (Unknown tag', y) :: remain when tag = tag' -> y
        | _, _ :: remain -> f remain
        | _, [] ->
            global := (tag, List.length !global) :: !global ;
            List.length !global - 1
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
        | PVar (id, ty) -> PVar (id, generalize_ty tbl level ty)
        | PTuple (ps, ty) ->
            PTuple (List.map f ps, List.map (generalize_ty tbl level) ty)
    in
    f

let generalize tbl level =
    let rec f = function
        | Int i -> Int i
        | Bool b -> Bool b
        | Var id -> Var id
        | App (g, args) -> App (f g, List.map f args)
        | Fun (args, body, ret_ty) ->
            Fun
              ( List.map
                  (fun (arg, ty) -> (arg, generalize_ty tbl level ty))
                  args
              , f body
              , generalize_ty tbl level ret_ty )
        | Tuple (vals, types) ->
            Tuple (List.map f vals, List.map (generalize_ty tbl level) types)
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
        | If (cond, e1, e2) -> If (f cond, f e1, f e2)
        | Match (target, target_ty, arms, ty) ->
            Match
              ( f target
              , generalize_ty tbl level target_ty
              , List.map
                  (fun (pat, expr) -> (generalize_pat tbl level pat, f expr))
                  arms
              , generalize_ty tbl level ty )
        | Ctor (name, t) -> Ctor (name, generalize_ty tbl level t)
        | CtorApp (name, e, t) ->
            CtorApp (name, List.map f e, generalize_ty tbl level t)
        | Never -> Never
    in
    f

type env_t = (id_t * Types.t) list [@@deriving show]

let rec pat_ty level = function
    | Ast.PVar name ->
        let ty = fresh level in
        ([([name], ty)], ty, PVar (name, ty))
    | Ast.PTuple ts ->
        let names, tys, ps = Util.unzip3 @@ List.map (pat_ty level) ts in
        (List.concat names, Types.Tuple tys, PTuple (ps, tys))
    | _ -> failwith @@ "unsupported pattern"

let rec canonical_type_def tenv co_def (name, targs, def) =
    (* envはtarg -> Types.tの写像*)
    (* Ast.typdef -> tydef_t *)
    let rec f_tydef hist (name, targs, tydef) =
        if List.for_all (fun n -> n <> name) hist then
          match tydef with
          | Ast.Alias ty -> f_ty (name :: hist) targs ty
          | Ast.Variant ctors -> failwith "unimplemented"
        else failwith "recursive definition"
    (* Ast.ty_t -> Types.t *)
    and f_ty hist env = function
        | Ast.TVar id -> map_to_ty id env
        | Ast.TApp (tys, name) -> (
          match lookup_from_co_def name co_def with
          | Some (targs, Ast.Variant _) ->
              (* TODO 型引数の長さを チェック *)
              Types.Variant (List.map (f_ty hist env) tys, name)
          | Some (targs, Ast.Alias alias) ->
              let targs =
                  Util.zip targs tys
                  |> List.map (fun (targ, ty) -> (targ, f_ty hist env ty))
              in
              f_tydef hist (name, targs, Ast.Alias alias)
          | None ->
              let higher = lookup_from_tenv name tenv in
              (* TODO 型引数の長さを チェック *)
              reassoc_ty
                (List.mapi (fun i ty -> (i, f_ty hist env ty)) tys)
                higher )
        | Ast.TInt -> Types.Int
        | Ast.TBool -> Types.Bool
        | Ast.TString -> Types.Str
        | Ast.TTuple ts -> Types.Tuple (List.map (f_ty hist env) ts)
    (* AbbreviationについてPolyを呼び出し元の型で置換 *)
    (* ここのenvはint -> Types.t *)
    and reassoc_ty env =
        let rec map i = function
            | (i', ty) :: _ when i = i' -> ty
            | _ :: remain -> map i remain
            | [] -> failwith @@ "oops!"
        in
        function
        | Types.Str -> Types.Str
        | Types.Int -> Types.Int
        | Types.Bool -> Types.Bool
        | Types.Poly i -> map i env
        | Types.Tuple ts -> Types.Tuple (List.map (reassoc_ty env) ts)
        | Types.Fun (args, ret) ->
            Types.Fun (List.map (reassoc_ty env) args, reassoc_ty env ret)
        | Types.Var _ -> failwith "oops!"
        | Types.Variant (args, name) ->
            Types.Variant (List.map (reassoc_ty env) args, name)
    and map_to_ty id = function
        | (id', ty) :: _ when id = id' -> ty
        | _ :: remain -> map_to_ty id remain
        | [] -> failwith @@ "undefined tvar '" ^ id
    and lookup_from_tenv id = function
        | (id', tydef) :: _ when id = id' -> tydef
        | _ :: remain -> lookup_from_tenv id remain
        | [] -> failwith @@ "undefined type" ^ Ast.show_id_t id
    and lookup_from_co_def id = function
        | (id', targs, ty) :: _ when id = id' -> Some (targs, ty)
        | _ :: remain -> lookup_from_co_def id remain
        | [] -> None
    in
    (* 先頭の型引数から順にPolyのIdを振る *)
    let targs = List.mapi (fun i targ -> (targ, Types.Poly i)) targs in
    match def with
    | Ast.Alias ty -> ([], f_tydef [] (name, targs, Ast.Alias ty))
    | Ast.Variant ctors ->
        let ctors =
            List.map
              (fun (ctor_name, args) ->
                ( [ctor_name]
                , (List.map (f_ty [] targs) args, List.map snd targs, name) ))
              ctors
        in
        (ctors, Types.Variant (List.map snd targs, name))

(* TODO: confirm one variable only bound once *)
let rec g env level =
    let venv, tenv, cenv = env in
    function
    | Ast.Int i -> (Int i, Types.Int)
    | Ast.Bool b -> (Bool b, Types.Bool)
    | Ast.Var id -> (Var id, lookup id venv)
    | Ast.App (f, args) -> (
        let args, arg_tys = Util.unzip @@ List.map (g env level) args in
        let arg_tys = List.map (instantiate (ref []) level) arg_tys in
        let f, f_ty = g env level f in
        let f_ty' = Types.Fun (arg_tys, fresh level) in
        let f_ty = instantiate (ref []) level f_ty in
        let f_ty = unify (instantiate (ref []) level f_ty) f_ty' in
        match f_ty with
        | Types.Fun (params, ret_ty) ->
            if List.length params > List.length args then
              let param_tys = Util.drop (List.length arg_tys) params in
              (App (f, args), Types.Fun (param_tys, ret_ty))
            else if List.length params = List.length args then
              (App (f, args), ret_ty)
            else failwith "too much argument"
        | _ -> failwith "unmatched app" )
    | Ast.Let (defs, expr) ->
        let pat_vars, defs =
            Util.unzip
            @@ List.map
                 (fun (pat, def) ->
                   let def, def_ty = g env (level + 1) def in
                   let pat_vars, pat_ty, pat = pat_ty (level + 1) pat in
                   unify pat_ty def_ty |> ignore ;
                   let tbl = ref [] in
                   let def, def_ty =
                       (generalize tbl level def, generalize_ty tbl level def_ty)
                   in
                   let pat, pat_ty =
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
        let expr, ty =
            g (List.concat pat_vars @ venv @ venv, tenv, cenv) level expr
        in
        (Let (defs, expr), ty)
    | Ast.LetRec (defs, expr) ->
        let vars = List.map (fun (name, _) -> (name, fresh (level + 1))) defs in
        let defs = List.map snd defs in
        let env = (vars @ venv, tenv, cenv) in
        let tbl = ref [] in
        let defs =
            Util.zip vars defs
            |> List.map (fun ((name, ty), def) ->
                   let def, def_ty = g env (level + 1) def in
                   (name, unify ty def_ty, def))
        in
        let defs =
            List.map
              (fun (name, ty, def) ->
                (name, generalize_ty tbl level ty, generalize tbl level def))
              defs
        in
        let expr, ty = g env level expr in
        (LetRec (defs, expr), ty)
    | Ast.Fun (args, body) ->
        let args = List.map (fun arg -> (arg, fresh level)) args in
        let arg_as_vars = List.map (fun (arg, ty) -> ([arg], ty)) args in
        let body, body_ty = g (arg_as_vars @ venv, tenv, cenv) level body in
        (Fun (args, body, body_ty), Types.Fun (List.map snd args, body_ty))
    | Ast.Tuple tp ->
        let elems, types = Util.unzip @@ List.map (g env level) tp in
        (Tuple (elems, types), Types.Tuple types)
    | Ast.If (cond, e1, e2) ->
        let cond, cond_ty = g env level cond in
        unify cond_ty Types.Bool |> ignore ;
        let e1, e1_ty = g env level e1 in
        let e2, e2_ty = g env level e2 in
        (If (cond, e1, e2), unify e1_ty e2_ty)
    | Ast.Never -> (Never, Types.Tuple [])
    | Ast.Ctor name -> (
      match lookup name cenv with
      | [], targs, variant_name ->
          let ty =
              instantiate (ref []) level (Types.Variant (targs, variant_name))
          in
          (Ctor (name, ty), ty)
      | t -> failwith @@ Printf.sprintf "Ctor must not have invalid type" )
    | Ast.CtorApp (name, args) -> (
      match lookup name cenv with
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
          (CtorApp (name, [Tuple (args, arg_tys)], variant_ty), variant_ty)
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
          (CtorApp (name, args, variant_ty), variant_ty) )
    | Ast.Match (target, arms) ->
        let target, target_ty = g env level target in
        let pats, guards, exprs =
            Util.unzip3
            @@ List.map
                 (fun (pat, guard, expr) ->
                   let tbl = ref [] in
                   let pat_vars, pat_ty, pat = pat_ty level pat in
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
            List.fold_left (fun t t' -> unify t t') target_ty pat_tys
        in
        let t =
            List.fold_left (fun t t' -> unify t t') (fresh level) expr_tys
        in
        List.map (fun t -> unify t Types.Bool) guard_tys |> ignore ;
        (Match (target, target_ty, Util.zip pats exprs, t), t)
    | Ast.Type (defs, expr) ->
        let names = List.map (fun (name, _, _) -> [name]) defs in
        let defs =
            List.map (fun (name, targs, tydef) -> ([name], targs, tydef)) defs
        in
        let defs = List.map (canonical_type_def tenv defs) defs in
        let ctors, tydefs = Util.unzip defs in
        let tydefs = Util.zip names tydefs in
        let ctors = List.concat ctors in
        let env = (venv, tydefs @ tenv, ctors @ cenv) in
        g env level expr

let f ast =
    fst
    @@ g
         (Types.pervasive_vals, Types.pervasive_types, Types.pervasive_ctors)
         0 ast
