type t =
    | Int of int
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Div of t * t
    | Mod of t * t
    | Neg of t
[@@deriving show]
