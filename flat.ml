type t =
    | Never
    | Int of int
    | Bool of bool
    | Var of Id.t * Types.t
    | CtorApp of Id.t * (t * Types.t) list * Types.t
    | Tuple of (t * Types.t) list
    | If of t * t * t * Types.t
    | Let of (Closure.pat_t * t * Types.t) list * t * Types.t
    | LetRec of (Id.t * t * Types.t) list * t * Types.t
    | Match of (t * Types.t) * ((Closure.pat_t * t * t) list) * Types.t
    | App of t * t * Types.t * Types.t
    | Fun of int * Types.t list * Types.t
[@@deriving show]
and fun_t = (Id.t * Types.t) list * t * Types.t
[@@deriving show]

let count = ref 0

let rec f funs = function
    | Closure.Int i -> Int i
    | Closure.Bool b -> Bool b
    | Closure.Never -> Never
    | Closure.Var (id, ty) -> Var (id, ty)
    | Closure.CtorApp (id, args, ty) -> CtorApp (id, List.map (fun (e, ty) -> f funs e, ty) args, ty)
    | Closure.Tuple es -> Tuple (List.map (fun (e, ty) -> f funs e, ty) es)
    | Closure.If (c, t, e, ty) -> If (f funs c, f funs t, f funs e, ty)
    | Closure.App (g, a, a_ty, ret_ty) -> App(f funs g, f funs a, a_ty, ret_ty)
    | Closure.Let (defs, expr, ty) -> Let (List.map (fun (pat, e, ty) -> pat, f funs e, ty) defs, f funs expr, ty)
    | Closure.LetRec (defs, expr, ty) -> LetRec (List.map (fun (id, e, ty) -> id, f funs e, ty) defs, f funs expr, ty)
    | Closure.Match ((target, target_ty), arms, ty) ->
        Match ((f funs target, target_ty), List.map (fun (pat, guard, expr) -> pat, f funs guard, f funs expr) arms, ty)
    | Closure.Fun (args, body, ret_ty) ->
        let id = !count in
        count := !count + 1;
        funs := (id, (args, body, ret_ty)) :: !funs;
        Fun (id, List.map snd args, ret_ty)

let f ast =
    let funs = ref [] in
    let ast = f funs ast in
    funs, ast
