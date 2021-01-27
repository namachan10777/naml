type t =
    | Emp
    | Int of int
    | Bool of bool
    | Var of string
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Div of t * t
    | Mod of t * t
    | Neg of t
    | Not of t
    | Eq of t * t
    | Neq of t * t
    | Or of t * t
    | And of t * t
    | Gret of t * t
    | Less of t * t
    | Cons of t * t
    | Tuple of t list
    | If of t * t * t
    | Let of string * t * t
    | Fun of string list * t
    | App of t * t
    | Seq of t * t
    | Pipeline of t * t
    | Paren of t
[@@deriving show]

let rec remove_paren = function
    | Emp -> Emp
    | If (cond, e1, e2) -> If (remove_paren cond, remove_paren e1, remove_paren e2)
    | Tuple elems -> Tuple (List.map remove_paren elems)
    | App (f, arg) -> App (remove_paren f, remove_paren arg)
    | Pipeline (arg, f) -> App (remove_paren f, remove_paren arg)
    | Paren e -> remove_paren e
    | Neg e -> Neg (remove_paren e)
    | Not e -> Not (remove_paren e)
    | Int i -> Int i
    | Bool b -> Bool b
    | Var id -> Var id
    | Seq (lhr, rhr) -> Seq (remove_paren lhr, remove_paren rhr)
    | Add (lhr, rhr) -> Add (remove_paren lhr, remove_paren rhr)
    | Sub (lhr, rhr) -> Sub (remove_paren lhr, remove_paren rhr)
    | Mul (lhr, rhr) -> Mul (remove_paren lhr, remove_paren rhr)
    | Div (lhr, rhr) -> Div (remove_paren lhr, remove_paren rhr)
    | Mod (lhr, rhr) -> Mod (remove_paren lhr, remove_paren rhr)
    | Eq (lhr, rhr) -> Eq (remove_paren lhr, remove_paren rhr)
    | Neq (lhr, rhr) -> Neq (remove_paren lhr, remove_paren rhr)
    | Or (lhr, rhr) -> Or (remove_paren lhr, remove_paren rhr)
    | And (lhr, rhr) -> And (remove_paren lhr, remove_paren rhr)
    | Gret (lhr, rhr) -> Gret (remove_paren lhr, remove_paren rhr)
    | Less (lhr, rhr) -> Less (remove_paren lhr, remove_paren rhr)
    | Cons (lhr, rhr) -> Cons (remove_paren lhr, remove_paren rhr)
    | Let (id, def, expr) -> Let (id, remove_paren def, remove_paren expr)
    | Fun (args, expr) -> Fun (args, remove_paren expr)
