type id_t = int

type map_t = string list * id_t

(* val * type * ctor * exception * record *)
type env_t = map_t * map_t * map_t

exception UnboundId of string list

type pat_t =
    | PInt of int
    | PBool of bool
    | PVar of id_t
    | PTuple of pat_t list
    | As of pat_t list
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
    | (name', id) :: _ when name = name' -> id
    | _ :: remain -> lookup name remain
    | [] -> raise @@ UnboundId name

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

let rec of_pat prefix = function
    | Ast.PBool b -> (PBool b, [])
    | Ast.PInt i -> (PInt i, [])
    | Ast.PEmp -> (PCtor (0 (* "[]" id *), []), [])
    | Ast.PCons (v, l) ->
        let v, names = of_pat prefix v in
        let l, names' = of_pat prefix l in
        (PCtor (1 (* "::" id *), [v; l]), names @ names')
    | Ast.PVar id ->
        let id' = fresh_v () in
        (PVar id', [(prefix @ [id], id')])
    | Ast.PTuple ps ->
        let ps, envs = Util.unzip @@ List.map (of_pat prefix) ps in
        (PTuple ps, List.concat envs)
    | Ast.As ps ->
        let ps, envs = Util.unzip @@ List.map (of_pat prefix) ps in
        (As ps, List.concat envs)
    | Ast.PCtor name ->
        let id = fresh_c () in
        (PCtor (id, []), [(prefix @ name, id)])
    | Ast.PCtorApp (name, args) ->
        let id = fresh_c () in
        let args, envs = Util.unzip @@ List.map (of_pat prefix) args in
        (PCtor (id, args), (prefix @ name, id) :: List.concat envs)

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

let rec of_tydef prefix env targs = function
    | Ast.Alias ty -> (Alias (of_ty env targs ty), [])
    | Ast.Variant ctors ->
        let cenv', ctors =
            Util.unzip
            @@ List.map
                 (fun (name, tys) ->
                   let id = fresh_c () in
                   ((prefix @ [name], id), (id, List.map (of_ty env targs) tys)))
                 ctors
        in
        (Variant ctors, cenv')

let rec of_expr prefix env =
    let venv, tenv, cenv = env in
    function
    | Ast.Int i -> Int i
    | Ast.Bool b -> Bool b
    | Ast.Never -> Never
    | Ast.Var name -> Var (lookup (prefix @ name) venv)
    | Ast.Ctor name -> Ctor (lookup (prefix @ name) cenv)
    | Ast.CtorApp (name, es) ->
        CtorApp (lookup (prefix @ name) cenv, List.map (of_expr prefix env) es)
    | Ast.Tuple es -> Tuple (List.map (of_expr prefix env) es)
    | Ast.If (ec, e1, e2) ->
        If (of_expr prefix env ec, of_expr prefix env e1, of_expr prefix env e2)
    | Ast.Let (defs, e) ->
        let envs, pats, defs =
            Util.unzip3
            @@ List.map
                 (fun (pat, def) ->
                   let pat, venv' = of_pat prefix pat in
                   (venv', pat, of_expr prefix (venv' @ venv, tenv, cenv) def))
                 defs
        in
        Let
          ( Util.zip pats defs
          , of_expr prefix (List.concat envs @ venv, tenv, cenv) e )
    | Ast.LetRec (defs, e) ->
        let ids = List.map (fun (n, _) -> (prefix @ n, fresh_v ())) defs in
        let env = (ids @ venv, tenv, cenv) in
        let exprs = List.map (fun (_, e) -> of_expr prefix env e) defs in
        LetRec
          ( List.map (fun ((_, id), e) -> (id, e)) (Util.zip ids exprs)
          , of_expr prefix env e )
    | Ast.Fun (args, body) ->
        let ids = List.map (fun n -> (prefix @ [n], fresh_v ())) args in
        let env = (ids @ venv, tenv, cenv) in
        Fun (List.map snd ids, of_expr prefix env body)
    | Ast.Match (target, arms) ->
        let target = of_expr prefix env target in
        let arms =
            List.map
              (fun (pat, guard, expr) ->
                let pat, venv' = of_pat prefix pat in
                (pat, of_expr prefix env guard, of_expr prefix env expr))
              arms
        in
        Match (target, arms)
    | Ast.App (f, args) ->
        App (of_expr prefix env f, List.map (of_expr prefix env) args)
    | Ast.Type (defs, e) ->
        let names = List.map (fun (n, _, _) -> ([n], fresh_t ())) defs in
        let env = (venv, names @ tenv, cenv) in
        let tys, ctors, targs_l =
            Util.unzip3
            @@ List.map
                 (fun (_, targs, ty) ->
                   let targs = List.mapi (fun i n -> (n, i)) targs in
                   let ty, ctors = of_tydef prefix env targs ty in
                   (ty, ctors, List.length targs))
                 defs
        in
        let env = (venv, names @ tenv, List.concat ctors @ cenv) in
        Type (Util.zip3 (List.map snd names) targs_l tys, of_expr prefix env e)
