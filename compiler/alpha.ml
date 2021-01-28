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

type t =
    | Emp
    | Int of int
    | Bool of bool
    | Var of id_t
    | Tuple of t list
    | If of t * t * t
    | Let of id_t * t * t
    | LetRec of id_t * t * t
    | Fun of id_t list * t
    | Match of t * (pat_t * t * t) list
    | App of t * t
    | ArrayAssign of t * t * t
[@@deriving show]

type ty_t = | TId of id_t
    | TForall of tvar_t
    | TTuple of ty_t list
    | TApp of ty_t * id_t
    | TVar of string
[@@deriving show]

type tydef_t =
    | Variant of (id_t * ty_t) list
    | Alias of ty_t
[@@deriving show]

type stmt_t =
    | LetS of (pat_t * t) list
    | LetRecS of (pat_t * t) list
    | Type of (id_t * tvar_t list * tydef_t) list
[@@deriving show]

let count = ref 0
let fresh () =
    count := 1 + !count;
    !count

type env_t = (string * id_t) list

let init () =
    count := List.length Types.initial_alpha_env;
    Types.initial_alpha_env

exception AlphaError of string

let rec lookup x = function
    | (y, id) :: remain -> if x = y then (x, id) else lookup x remain
    | [] -> raise @@ AlphaError ("lookup failed: " ^ (List.fold_left (fun acc x -> acc ^ "." ^ x) (List.hd x) (List.tl x)))

let register env id =
    env := id :: !env;
    ()

let gen name = 
    (name, fresh ())

let unimplemented () = raise @@ Failure "unimplemented"

let rec of_pat env = function
    | Ast.PEmp -> PEmp, env
    | Ast.PVar name ->
        let id = gen [name] in
        PVar id, id :: env
    | Ast.PCtor name -> PCtor (lookup name env), env
    | Ast.PCtorApp (name, arg) ->
        let arg, env = of_pat env arg in
        PCtorApp (lookup name env, arg), env
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
let rec of_expr env = function
    | Ast.Emp -> Emp
    | Ast.Var id -> Var (lookup id env)
    | Ast.Bool b -> Bool b
    | Ast.Int i -> Int i
    | Ast.App (f, arg) -> App (of_expr env f, of_expr env arg)
    | Ast.Tuple ts -> Tuple (List.map (of_expr env) ts)
    | Ast.If (cond, e1, e2) -> If (of_expr env cond, of_expr env e1, of_expr env e2)
    | Ast.Let (name, def, expr) ->
        let id = gen [name] in
        Let (id, of_expr env def, of_expr (id :: env) expr)
    | Ast.LetRec (name, def, expr) ->
        let id = gen [name] in
        Let (id, of_expr (id :: env) def, of_expr (id :: env) expr)
    | Ast.Fun (args, body) ->
        let args = args |> List.map (fun n -> [n]) |> List.map gen in
        Fun (args, of_expr (args @ env) body)
    | Match (target, arms) ->
        let arms = List.map (fun (pat, guard, expr) -> 
            let pat, env = of_pat env pat in
            (pat, of_expr env guard, of_expr env expr)
        ) arms in
        Match (of_expr env target, arms)
    | ArrayAssign (target, idx, value) -> ArrayAssign (of_expr env target, of_expr env idx, of_expr env value)

let rec of_ty env = function
    | Ast.TTuple ts -> TTuple (List.map (of_ty env) ts)
    | Ast.TId id -> TId (lookup id env)
    | Ast.TVar v -> TVar v
    | Ast.TApp (t, id) -> TApp (of_ty env t, lookup id env)

let of_tydef env = function
    | Ast.Alias ty -> Alias (of_ty env ty)
    | Ast.Variant arms ->
        let names = List.map (fun arm -> gen [fst arm]) arms in
        let ty = List.map (fun (_, ty) -> of_ty env ty) arms in
        Variant (Util.zip names ty)

let of_stmt env = function
    | Ast.LetRecS defs ->
        let pats = List.map fst defs in
        let exprs = List.map snd defs in
        let pats, env = List.fold_left (fun (pats, env) pat ->
            let pat, env = of_pat env pat in
            (pat :: pats, env))
            ([], env)
            pats in
        let exprs = List.map (of_expr env) exprs in
        LetRecS (Util.zip pats exprs), env
    | Ast.LetS defs ->
        let pats = List.map fst defs in
        let exprs = List.map snd defs in
        let pats, env' = List.fold_left (fun (pats, env) pat ->
            let pat, env = of_pat env pat in
            (pat :: pats, env))
            ([], env)
            pats in
        let exprs = List.map (of_expr env) exprs in
        LetS (Util.zip pats exprs), env'
    | Ast.Type defs ->
        let names = List.map (fun (n, _, _) -> gen [n]) defs in
        let targss = List.map (fun (n, targs, _) -> targs) defs in
        let env = names @ env in
        let tys = List.map (fun (_, _, tydef) -> of_tydef env tydef) defs in
        Type (Util.zip3 names targss tys), env

type stmts_t = stmt_t list
[@@deriving show]

let of_stmts ss =
    let env = init () in
    snd @@ List.fold_left (fun (env, ss) s -> let s, env = of_stmt env s in (env, s :: ss)) (env, []) ss

let f fname src = of_stmts @@ Ast.f fname src
