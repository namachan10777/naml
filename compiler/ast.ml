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
