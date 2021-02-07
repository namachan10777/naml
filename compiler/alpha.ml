type 'a map_t = string list * 'a * bool

(* val * type * ctor * exception * record *)
type env_t = Types.vid_t map_t * Types.tid_t map_t * Types.cid_t map_t

exception UnboundId of string list * Lex.pos_t

exception DuplicatedId of string list * Lex.pos_t

exception BothSideOfOrPatternMustHaveSameVars of Lex.pos_t

type pat_t =
    | PInt of int * Lex.pos_t
    | PBool of bool * Lex.pos_t
    | PVar of Types.vid_t * Lex.pos_t
    | PTuple of pat_t list * Lex.pos_t
    | As of pat_t list * Lex.pos_t
    | Or of pat_t * pat_t list * Lex.pos_t
    | PCtor of Types.cid_t * pat_t list * Lex.pos_t
[@@deriving show]

type ty_t =
    | TInt of Lex.pos_t
    | TBool of Lex.pos_t
    | TString of Lex.pos_t
    | TVar of int * Lex.pos_t
    | TTuple of ty_t list * Lex.pos_t
    | TApp of ty_t list * Types.tid_t * Lex.pos_t
[@@deriving show]

type tydef_t =
    | Variant of (Types.cid_t * Lex.pos_t * ty_t list) list
    | Alias of ty_t
[@@deriving show]

type t =
    | Never
    | Int of int * Lex.pos_t
    | Bool of bool * Lex.pos_t
    | Var of Types.vid_t * Lex.pos_t
    | Ctor of Types.cid_t * Lex.pos_t
    | CtorApp of Types.cid_t * Lex.pos_t * t list
    | Tuple of t list * Lex.pos_t
    | If of t * t * t * Lex.pos_t
    | Let of (pat_t * t) list * t
    | LetRec of (Types.vid_t * Lex.pos_t * t) list * t
    | Fun of Types.vid_t list * t * Lex.pos_t
    | Match of t * (pat_t * t * t) list
    | App of t * t list * Lex.pos_t
    | Type of (Types.tid_t * Lex.pos_t * int * tydef_t) list * t
[@@deriving show]

let rec lookup p name = function
    | (name', id, _) :: _ when name = name' -> id
    | _ :: remain -> lookup p name remain
    | [] -> raise @@ UnboundId (name, p)

let pervasive_tenv =
    List.map (fun (name, id, _) -> (name, id, false)) Types.pervasive_types

let pervasive_venv =
    List.map (fun (name, id, _) -> (name, id, false)) Types.pervasive_vals

let pervasive_cenv =
    List.map (fun (name, id, _) -> (name, id, false)) Types.pervasive_ctors

let count_v = ref 0

let fresh_v () =
    count_v := !count_v + 1 ;
    Types.Vid !count_v

let count_c = ref 0

let fresh_c () =
    count_c := !count_c + 1 ;
    Types.Cid !count_c

let count_t = ref 0

let fresh_t () =
    count_t := !count_t + 1 ;
    Types.Tid !count_t

let init () =
    count_v := List.length pervasive_venv - 1 ;
    count_t := List.length pervasive_tenv - 1 ;
    count_c := List.length pervasive_cenv - 1 ;
    (pervasive_venv, pervasive_tenv, pervasive_cenv)

let rec dup_chk = function
    | [] -> None
    | (x, p) :: xs ->
        if not @@ List.for_all (fun (x', _) -> x' <> x) xs then Some (x, p)
        else dup_chk xs

let get1 (x, _, _) = x

let assert_dup ids =
    match dup_chk ids with
    | Some (id, p) -> raise @@ DuplicatedId (id, p)
    | None -> ()

let drop_pos = List.map (fun (name, _, id, is_local) -> (name, id, is_local))

let rec of_pat vtbl cenv =
    let rec lookup_vtbl p name = function
        | (name', id, _) :: _ when name = name' -> id
        | _ :: remain -> lookup_vtbl p name remain
        | _ -> raise @@ UnboundId (name, p)
    in
    function
    | Ast.PBool (b, p) -> (PBool (b, p), [])
    | Ast.PInt (i, p) -> (PInt (i, p), [])
    | Ast.PEmp p -> (PCtor (lookup p ["[]"] pervasive_cenv, [], p), [])
    | Ast.PCons (v, l, p) ->
        let v, names = of_pat vtbl cenv v in
        let l, names' = of_pat vtbl cenv l in
        (PCtor (lookup p ["::"] pervasive_cenv, [v; l], p), names @ names')
    | Ast.PVar (id, p) -> (
      match vtbl with
      | Some vtbl ->
          let id' = lookup_vtbl p [id] vtbl in
          (PVar (id', p), [])
      | None ->
          let id' = fresh_v () in
          (PVar (id', p), [([id], p, id', true)]) )
    | Ast.PTuple (ps, p) ->
        let ps, envs = Util.unzip @@ List.map (of_pat vtbl cenv) ps in
        (PTuple (ps, p), List.concat envs)
    | Ast.As (ps, p) ->
        let ps, envs = Util.unzip @@ List.map (of_pat vtbl cenv) ps in
        (As (ps, p), List.concat envs)
    | Ast.Or (pat, pats, p) ->
        let pat, venv = of_pat vtbl cenv pat in
        assert_dup @@ List.map (fun (id, p, _, _) -> (id, p)) venv ;
        let vtbl =
            match vtbl with Some v -> Some v | None -> Some (drop_pos venv)
        in
        let pats, venvs = Util.unzip @@ List.map (of_pat vtbl cenv) pats in
        if
          List.for_all
            (fun venv' -> List.sort compare venv' = List.sort compare venv)
            venvs
        then (Or (pat, pats, p), venv)
        else raise @@ BothSideOfOrPatternMustHaveSameVars p
    | Ast.PCtor (name, p) ->
        let id = lookup p name cenv in
        (PCtor (id, [], p), [])
    | Ast.PCtorApp (name, args, p) ->
        let id = lookup p name cenv in
        let args, envs = Util.unzip @@ List.map (of_pat vtbl cenv) args in
        (PCtor (id, args, p), List.concat envs)

let rec of_ty env targs =
    let rec lookup_targs p arg = function
        | (arg', id) :: _ when arg = arg' -> id
        | _ :: remain -> lookup_targs p arg remain
        | [] -> raise @@ UnboundId (["'" ^ arg], p)
    in
    let _, tenv, _ = env in
    function
    | Ast.TInt p -> TInt p
    | Ast.TBool p -> TBool p
    | Ast.TString p -> TString p
    | Ast.TVar (v, p) -> TVar (lookup_targs p v targs, p)
    | Ast.TTuple (ts, p) -> TTuple (List.map (of_ty env targs) ts, p)
    | Ast.TApp (ts, higher, p) ->
        TApp (List.map (of_ty env targs) ts, lookup p higher tenv, p)

let rec of_tydef env targs = function
    | Ast.Alias ty -> (Alias (of_ty env targs ty), [])
    | Ast.Variant ctors ->
        let cenv', ctors =
            Util.unzip
            @@ List.map
                 (fun (name, p, tys) ->
                   let id = fresh_c () in
                   (([name], id, true), (id, p, List.map (of_ty env targs) tys)))
                 ctors
        in
        (Variant ctors, cenv')

let rec of_expr env =
    let venv, tenv, cenv = env in
    function
    | Ast.Int (i, p) -> Int (i, p)
    | Ast.Bool (b, p) -> Bool (b, p)
    | Ast.Never -> Never
    | Ast.Var (name, p) -> Var (lookup p name venv, p)
    | Ast.Ctor (name, p) -> Ctor (lookup p name cenv, p)
    | Ast.CtorApp (name, p, es) ->
        CtorApp (lookup p name cenv, p, List.map (of_expr env) es)
    | Ast.Tuple (es, p) -> Tuple (List.map (of_expr env) es, p)
    | Ast.If (ec, e1, e2, p) ->
        If (of_expr env ec, of_expr env e1, of_expr env e2, p)
    | Ast.Let (defs, e) ->
        let pat_vars, pats, defs =
            Util.unzip3
            @@ List.map
                 (fun (pat, def) ->
                   let pat, venv' = of_pat None cenv pat in
                   (venv', pat, of_expr (drop_pos venv' @ venv, tenv, cenv) def))
                 defs
        in
        let venv' = List.concat pat_vars in
        assert_dup @@ List.map (fun (id, p, _, _) -> (id, p)) venv' ;
        Let (Util.zip pats defs, of_expr (drop_pos venv' @ venv, tenv, cenv) e)
    | Ast.LetRec (defs, e) ->
        let ids = List.map (fun (n, p, _) -> (n, p, fresh_v (), true)) defs in
        assert_dup @@ List.map (fun (id, p, _, _) -> (id, p)) ids ;
        let env = (drop_pos ids @ venv, tenv, cenv) in
        let exprs = List.map (fun (_, _, e) -> of_expr env e) defs in
        let ps = List.map (fun (_, p, _) -> p) defs in
        LetRec
          ( List.map
              (fun ((_, id, _), p, e) -> (id, p, e))
              (Util.zip3 (drop_pos ids) ps exprs)
          , of_expr env e )
    | Ast.Fun (args, body, p) ->
        let ids = List.map (fun (n, _) -> ([n], fresh_v (), true)) args in
        let env = (ids @ venv, tenv, cenv) in
        Fun (List.map (fun (_, i, _) -> i) ids, of_expr env body, p)
    | Ast.Match (target, arms) ->
        let target = of_expr env target in
        let arms =
            List.map
              (fun (pat, guard, expr) ->
                let pat, venv' = of_pat None cenv pat in
                assert_dup @@ List.map (fun (id, p, _, _) -> (id, p)) venv' ;
                let env = (drop_pos venv' @ venv, tenv, cenv) in
                (pat, of_expr env guard, of_expr env expr))
              arms
        in
        Match (target, arms)
    | Ast.App (f, args, p) -> App (of_expr env f, List.map (of_expr env) args, p)
    | Ast.Type (defs, e) ->
        let names =
            List.map (fun (n, _, _, _) -> ([n], fresh_t (), true)) defs
        in
        let env = (venv, names @ tenv, cenv) in
        let tys, ps, ctors, targs_l =
            Util.unzip4
            @@ List.map
                 (fun (_, p, targs, ty) ->
                   let targs = List.mapi (fun i (n, _) -> (n, i)) targs in
                   let ty, ctors = of_tydef env targs ty in
                   (ty, p, ctors, List.length targs))
                 defs
        in
        assert_dup @@ List.map (fun (id, p, _, _) -> ([id], p)) defs ;
        let env = (venv, names @ tenv, List.concat ctors @ cenv) in
        Type
          ( Util.zip4 (List.map (fun (_, i, _) -> i) names) ps targs_l tys
          , of_expr env e )
