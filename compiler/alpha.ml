type id_t = int [@@deriving show]

type map_t = string list * id_t * bool

(* val * type * ctor * exception * record *)
type env_t = map_t * map_t * map_t

exception UnboundId of string list

exception DuplicatedId of string list
exception BothSideOfOrPatternMustHaveSameVars

type pat_t =
    | PInt of int
    | PBool of bool
    | PVar of id_t
    | PTuple of pat_t list
    | As of pat_t list
    | Or of pat_t * pat_t list
    | PCtor of id_t * pat_t list
[@@deriving show]

type ty_t =
    | TInt
    | TBool
    | TString
    | TVar of id_t
    | TTuple of ty_t list
    | TApp of ty_t list * id_t
[@@deriving show]

type tydef_t = Variant of (id_t * ty_t list) list | Alias of ty_t
[@@deriving show]

type t =
    | Never
    | Int of int
    | Bool of bool
    | Var of id_t
    | Ctor of id_t
    | CtorApp of id_t * t list
    | Tuple of t list
    | If of t * t * t
    | Let of (pat_t * t) list * t
    | LetRec of (id_t * t) list * t
    | Fun of id_t list * t
    | Match of t * (pat_t * t * t) list
    | App of t * t list
    | Type of (id_t * int * tydef_t) list * t
[@@deriving show]

let rec lookup name = function
    | (name', id, _) :: _ when name = name' -> id
    | _ :: remain -> lookup name remain
    | [] -> raise @@ UnboundId name

let pervasive_tenv =
    List.map (fun (name, id, _) -> (name, id, false)) Types.pervasive_types

let pervasive_venv =
    List.map (fun (name, id, _) -> (name, id, false)) Types.pervasive_vals

let pervasive_cenv =
    List.map (fun (name, id, _) -> (name, id, false)) Types.pervasive_ctors

let count_v = ref 0

let fresh_v () =
    count_v := !count_v + 1 ;
    !count_v

let count_c = ref 0

let fresh_c () =
    count_c := !count_c + 1 ;
    !count_c

let count_t = ref 0

let fresh_t () =
    count_t := !count_t + 1 ;
    !count_t

let init () =
    count_v := List.length pervasive_venv - 1 ;
    count_t := List.length pervasive_tenv - 1 ;
    count_c := List.length pervasive_cenv - 1 ;
    (pervasive_venv, pervasive_tenv, pervasive_cenv)

let rec dup_chk = function
    | [] -> None
    | x :: xs ->
        if not @@ List.for_all (( <> ) x) xs then Some x else dup_chk xs

let get1 (x, _, _) = x

let assert_dup ids =
    match dup_chk ids with Some id -> raise @@ DuplicatedId id | None -> ()

let rec of_pat cenv = function
    | Ast.PBool b -> (PBool b, [])
    | Ast.PInt i -> (PInt i, [])
    | Ast.PEmp -> (PCtor (lookup ["[]"] pervasive_cenv, []), [])
    | Ast.PCons (v, l) ->
        let v, names = of_pat cenv v in
        let l, names' = of_pat cenv l in
        (PCtor (lookup ["::"] pervasive_cenv, [v; l]), names @ names')
    | Ast.PVar id ->
        let id' = fresh_v () in
        (PVar id', [([id], id', true)])
    | Ast.PTuple ps ->
        let ps, envs = Util.unzip @@ List.map (of_pat cenv) ps in
        (PTuple ps, List.concat envs)
    | Ast.As ps ->
        let ps, envs = Util.unzip @@ List.map (of_pat cenv) ps in
        (As ps, List.concat envs)
    | Ast.Or (p, ps) ->
        let p, venv = of_pat cenv p in
        let count_v_snapshot = !count_v in
        assert_dup @@ List.map get1 venv;
        let ps, venvs = Util.unzip @@ List.map (of_pat cenv) ps in
        count_v := count_v_snapshot;
        if List.for_all (fun venv' -> (List.sort compare venv') = (List.sort compare venv)) venvs
        then (Or (p, ps), venv)
        else raise BothSideOfOrPatternMustHaveSameVars
    | Ast.PCtor name ->
        let id = fresh_c () in
        (PCtor (id, []), [(name, id, true)])
    | Ast.PCtorApp (name, args) ->
        let id = fresh_c () in
        let args, envs = Util.unzip @@ List.map (of_pat cenv) args in
        (PCtor (id, args), (name, id, true) :: List.concat envs)

let rec of_ty env targs =
    let rec lookup_targs arg = function
        | (arg', id) :: _ when arg = arg' -> id
        | _ :: remain -> lookup_targs arg remain
        | [] -> raise @@ UnboundId ["'" ^ arg]
    in
    let _, tenv, _ = env in
    function
    | Ast.TInt -> TInt
    | Ast.TBool -> TBool
    | Ast.TString -> TString
    | Ast.TVar v -> TVar (lookup_targs v targs)
    | Ast.TTuple ts -> TTuple (List.map (of_ty env targs) ts)
    | Ast.TApp (ts, higher) ->
        TApp (List.map (of_ty env targs) ts, lookup higher tenv)

let rec of_tydef env targs = function
    | Ast.Alias ty -> (Alias (of_ty env targs ty), [])
    | Ast.Variant ctors ->
        let cenv', ctors =
            Util.unzip
            @@ List.map
                 (fun (name, tys) ->
                   let id = fresh_c () in
                   (([name], id, true), (id, List.map (of_ty env targs) tys)))
                 ctors
        in
        (Variant ctors, cenv')

let rec of_expr env =
    let (venv : (string list * int * bool) list), tenv, cenv = env in
    function
    | Ast.Int i -> Int i
    | Ast.Bool b -> Bool b
    | Ast.Never -> Never
    | Ast.Var name -> Var (lookup name venv)
    | Ast.Ctor name -> Ctor (lookup name cenv)
    | Ast.CtorApp (name, es) ->
        CtorApp (lookup name cenv, List.map (of_expr env) es)
    | Ast.Tuple es -> Tuple (List.map (of_expr env) es)
    | Ast.If (ec, e1, e2) -> If (of_expr env ec, of_expr env e1, of_expr env e2)
    | Ast.Let (defs, e) ->
        let pat_vars, pats, defs =
            Util.unzip3
            @@ List.map
                 (fun (pat, def) ->
                   let pat, venv' = of_pat cenv pat in
                   (venv', pat, of_expr (venv' @ venv, tenv, cenv) def))
                 defs
        in
        let venv' = List.concat pat_vars in
        assert_dup @@ List.map get1 venv' ;
        Let (Util.zip pats defs, of_expr (venv' @ venv, tenv, cenv) e)
    | Ast.LetRec (defs, e) ->
        let ids = List.map (fun (n, _) -> (n, fresh_v (), true)) defs in
        assert_dup @@ List.map get1 ids ;
        let env = (ids @ venv, tenv, cenv) in
        let exprs = List.map (fun (_, e) -> of_expr env e) defs in
        LetRec
          ( List.map (fun ((_, id, _), e) -> (id, e)) (Util.zip ids exprs)
          , of_expr env e )
    | Ast.Fun (args, body) ->
        let ids = List.map (fun n -> ([n], fresh_v (), true)) args in
        let env = (ids @ venv, tenv, cenv) in
        Fun (List.map (fun (_, i, _) -> i) ids, of_expr env body)
    | Ast.Match (target, arms) ->
        let target = of_expr env target in
        let arms =
            List.map
              (fun (pat, guard, expr) ->
                let pat, venv' = of_pat cenv pat in
                assert_dup @@ List.map get1 venv' ;
                let env = (venv' @ venv, tenv, cenv) in
                (pat, of_expr env guard, of_expr env expr))
              arms
        in
        Match (target, arms)
    | Ast.App (f, args) -> App (of_expr env f, List.map (of_expr env) args)
    | Ast.Type (defs, e) ->
        let names = List.map (fun (n, _, _) -> ([n], fresh_t (), true)) defs in
        let env = (venv, names @ tenv, cenv) in
        let tys, ctors, targs_l =
            Util.unzip3
            @@ List.map
                 (fun (_, targs, ty) ->
                   let targs = List.mapi (fun i n -> (n, i)) targs in
                   let ty, ctors = of_tydef env targs ty in
                   (ty, ctors, List.length targs))
                 defs
        in
        assert_dup @@ List.map get1 names ;
        let env = (venv, names @ tenv, List.concat ctors @ cenv) in
        Type
          ( Util.zip3 (List.map (fun (_, i, _) -> i) names) targs_l tys
          , of_expr env e )
