type t =
    | Never
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Div of t * t
    | Mod of t * t
    | Gret of t * t
    | Less of t * t
    | Eq of t * t * Types.t
    | Neq of t * t * Types.t
    | Or of t * t
    | And of t * t
    | Append of t * t * Types.t
    | ArrayAssign of t * t * t * Types.t
    | Assign of t * t * Types.t
    | Ref of t * Types.t
    | Not of t
    | Neg of t
    | ArrayAccess of t * t * Types.t
    | PrintString of t
    | PrintInt of t
    | Failwith of t
    | Int of int
    | Bool of bool
    | Var of Id.t * Types.t
    | CtorApp of Id.t * (t * Types.t) list * Types.t
    | Tuple of (t * Types.t) list
    | If of t * t * t * Types.t
    | Let of (Flatlet.pat_t * t * Types.t) * t * Types.t
    | Match of (t * Types.t) * ((Flatlet.pat_t * t * t) list) * Types.t
    | App of t * t * Types.t * Types.t
    | Fun of int * Types.t list * Types.t
[@@deriving show]
and fun_t = (Id.t * Types.t) list * t * Types.t
[@@deriving show]
type funs_t = (int * fun_t) list
[@@deriving show]
type stmts_t = (Flatlet.pat_t * t * Types.t) list
[@@deriving show]

let count = ref 0

let rec f funs = function
    | Closure.Int i -> Int i
    | Closure.Bool b -> Bool b
    | Closure.Never -> Never
    | Closure.Var (id, ty) -> Var (id, ty)
    | Closure.Add (lhr, rhr) -> Add (f funs lhr, f funs rhr)
    | Closure.Sub (lhr, rhr) -> Sub (f funs lhr, f funs rhr)
    | Closure.Mul (lhr, rhr) -> Mul (f funs lhr, f funs rhr)
    | Closure.Div (lhr, rhr) -> Div (f funs lhr, f funs rhr)
    | Closure.Mod (lhr, rhr) -> Mod (f funs lhr, f funs rhr)
    | Closure.Gret (lhr, rhr) -> Gret (f funs lhr, f funs rhr)
    | Closure.Less (lhr, rhr) -> Less (f funs lhr, f funs rhr)
    | Closure.Eq (lhr, rhr, ty) -> Eq (f funs lhr, f funs rhr, ty)
    | Closure.Neq (lhr, rhr, ty) -> Neq (f funs lhr, f funs rhr, ty)
    | Closure.Append (lhr, rhr, ty) -> Append (f funs lhr, f funs rhr, ty)
    | Closure.Assign (lhr, rhr, ty) -> Assign (f funs lhr, f funs rhr, ty)
    | Closure.ArrayAccess (lhr, rhr, ty) -> ArrayAccess (f funs lhr, f funs rhr, ty)
    | Closure.ArrayAssign (target, idx, inner, ty) -> ArrayAssign (f funs target, f funs idx, f funs inner, ty)
    | Closure.Or (lhr, rhr) -> Or (f funs lhr, f funs rhr)
    | Closure.And (lhr, rhr) -> And (f funs lhr, f funs rhr)
    | Closure.Ref (e, ty) -> Ref (f funs e, ty)
    | Closure.Not e -> Not (f funs e)
    | Closure.Neg e -> Neg (f funs e)
    | Closure.PrintInt e -> PrintInt (f funs e)
    | Closure.PrintString e -> PrintString (f funs e)
    | Closure.Failwith e -> Failwith (f funs e)
    | Closure.CtorApp (id, args, ty) -> CtorApp (id, List.map (fun (e, ty) -> f funs e, ty) args, ty)
    | Closure.Tuple es -> Tuple (List.map (fun (e, ty) -> f funs e, ty) es)
    | Closure.If (c, t, e, ty) -> If (f funs c, f funs t, f funs e, ty)
    | Closure.App (g, a, a_ty, ret_ty) -> App(f funs g, f funs a, a_ty, ret_ty)
    | Closure.Let ((pat, def, def_ty), expr, ty) -> Let ((pat, f funs def, def_ty), f funs expr, ty)
    | Closure.Match (target, target_ty, arms, ty) ->
        Match ((f funs target, target_ty), List.map (fun (pat, guard, expr) -> pat, f funs guard, f funs expr) arms, ty)
    | Closure.Fun (args, body, ret_ty) ->
        let id = !count in
        count := !count + 1;
        funs := (id, (args, f funs body, ret_ty)) :: !funs;
        Fun (id, List.map snd args, ret_ty)

let f ast =
    let funs = ref [] in
    let ast = List.map (fun (p, def, ty) -> p, f funs def, ty) ast in
    !funs, ast
