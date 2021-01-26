type t =
    | Int of int
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Div of t * t
    | Mod of t * t
[@@deriving show]
