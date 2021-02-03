type id_t = string list * int 
[@@deriving show]

type tvar_t = string
[@@deriving show]

type pat_t =
    | PEmp
    | PCons of pat_t * pat_t
    | PInt of int
    | PBool of bool
    | PVar of id_t
    | PTuple of pat_t list
    | As of pat_t list
    | PCtorApp of id_t * pat_t
    | PCtor of id_t
[@@deriving show]

type ty_t =
    | TForall of tvar_t
    | TTuple of ty_t list
    | TApp of ty_t list * id_t
    | TVar of string
[@@deriving show]

type tydef_t =
    | Variant of (id_t * ty_t list) list
    | Alias of ty_t
[@@deriving show]

type t =
    | Emp
    | Never
    | Int of int
    | Bool of bool
    | Var of id_t
    | Tuple of t list
    | If of t * t * t
    | Let of (pat_t * t) list * t
    | LetRec of (id_t * t) list * t
    | Type of (id_t * string list * tydef_t) list * t
    | Fun of id_t list * t
    | Match of t * (pat_t * t * t) list
    | App of t * t list
    | CtorApp of id_t * t list
    | ArrayAssign of t * t * t
[@@deriving show]

let count_vals = ref @@ (List.length Types.pervasive_val_ids) - 1
let count_types = ref @@ (List.length Types.pervasive_type_ids) - 1
let count_ctors = ref @@ (List.length Types.pervasive_ctor_ids) - 1

type env_t = (string * id_t) list

let init () =
    count_vals  := (List.length Types.pervasive_val_ids ) - 1;
    count_types := (List.length Types.pervasive_type_ids) - 1;
    count_ctors := (List.length Types.pervasive_ctor_ids) - 1;
    Types.pervasive_val_ids, Types.pervasive_type_ids, Types.pervasive_ctor_ids

exception AlphaError of string

let rec lookup x = function
    | (y, id) :: remain -> if x = y then (x, id) else lookup x remain
    | [] -> raise @@ AlphaError ("lookup failed: " ^ (List.fold_left (fun acc x -> acc ^ "." ^ x) (List.hd x) (List.tl x)))

let val_id name = 
    count_vals := 1 + !count_vals;
    (name, !count_vals)

let type_id name = 
    count_types := 1 + !count_types;
    (name, !count_types)

let ctor_id name = 
    count_ctors := 1 + !count_ctors;
    (name, !count_ctors)

let unimplemented () = raise @@ Failure "unimplemented"

let rec of_ty env =
    let venv, tenv, cenv = env in
    function
    | Ast.TTuple ts -> TTuple (List.map (of_ty env) ts)
    | Ast.TVar v -> TVar v
    | Ast.TApp (ts, id) -> TApp (List.map (of_ty env) ts, lookup id tenv)

let of_tydef env =
    let venv, tenv, cenv = env in
    function
    | Ast.Alias ty -> Alias (of_ty env ty), env
    | Ast.Variant arms ->
        let names = List.map (fun arm -> type_id [fst arm]) arms in
        let ty = List.map (fun (_, ty) -> List.map (of_ty env) ty) arms in
        Variant (Util.zip names ty), (venv, tenv, names @ cenv)

let rec of_pat env =
    let venv, tenv, cenv = env in
    function
    | Ast.PEmp -> PEmp, env
    | Ast.PVar name ->
        let id = val_id [name] in
        PVar id, (id :: venv, tenv, cenv)
    | Ast.PCtor name -> PCtor (lookup name cenv), env
    | Ast.PCtorApp (name, arg) ->
        let arg, env = of_pat env arg in
        PCtorApp (lookup name cenv, arg), env
    | Ast.PBool b -> PBool b, env
    | Ast.PInt i -> PInt i, env
    | Ast.PTuple pats ->
        let (pats, env) = List.fold_left (fun (pats, env) pat ->
            let (pat, env) = of_pat env pat in
            (pat :: pats, env)) ([], env) pats in
        PTuple pats, env
    | Ast.As pats ->
        let (pats, env) = List.fold_left (fun (pats, env) pat ->
            let (pat, env) = of_pat env pat in
            (pat :: pats, env)) ([], env) pats in
        As pats, env
    | Ast.PCons (lhr, rhr) ->
        let (lhr, env) = of_pat env lhr in
        let (rhr, env) = of_pat env rhr in
        PCons (lhr, rhr), env

let rec gather_free_ids pat = unimplemented ()
let rec of_expr env =
    let venv, tenv, cenv = env in
    function
    | Ast.Never -> Never
    | Ast.Var id -> Var (lookup id venv)
    | Ast.Ctor id -> Var (lookup id cenv)
    | Ast.Bool b -> Bool b
    | Ast.Int i -> Int i
    | Ast.App (f, arg) -> App (of_expr env f, List.map (of_expr env) arg)
    | Ast.CtorApp (id, arg) -> CtorApp (lookup id venv, List.map (of_expr env) arg)
    | Ast.Tuple ts -> Tuple (List.map (of_expr env) ts)
    | Ast.If (cond, e1, e2) -> If (of_expr env cond, of_expr env e1, of_expr env e2)
    | Ast.Fun (args, body) ->
        let args = args |> List.map (fun n -> [n]) |> List.map val_id in
        Fun (args, of_expr (args @ venv, tenv, cenv) body)
    | Match (target, arms) ->
        let arms = List.map (fun (pat, guard, expr) -> 
            let pat, env = of_pat env pat in
            (pat, of_expr env guard, of_expr env expr)
        ) arms in
        Match (of_expr env target, arms)
    | Ast.LetRec (defs, expr) ->
        let ids = List.map fst defs in
        let exprs = List.map snd defs in
        let ids = List.map (fun id -> val_id id) ids in
        let env = (ids @ venv, tenv, cenv) in
        let exprs = List.map (of_expr env) exprs in
        LetRec (Util.zip ids exprs, of_expr env expr)
    | Ast.Let (defs, expr) ->
        let pats = List.map fst defs in
        let exprs = List.map snd defs in
        let pats, env' = List.fold_left (fun (pats, env) pat ->
            let pat, env = of_pat env pat in
            (pat :: pats, env))
            ([], env)
            pats in
        let exprs = List.map (of_expr env) exprs in
        Let (Util.zip pats exprs, of_expr env' expr)
    | Ast.Type (defs, expr) ->
        let names = List.map (fun (n, _, _) -> val_id [n]) defs in
        let targss = List.map (fun (n, targs, _) -> targs) defs in
        let env = (venv, names @ tenv, cenv) in
        let tys, env = List.fold_left (fun (tys, env) (_, _, tydef) ->
            let ty, env = of_tydef env tydef in
            ty :: tys, env
        ) ([], env) defs in
        Type (Util.zip3 names targss tys, of_expr env expr)


let f fname src = of_expr (init ()) @@ Ast.f fname src
