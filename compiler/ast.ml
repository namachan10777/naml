type t =
    | Int of int
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Div of t * t
    | Mod of t * t
    | Neg of t
    | Paren of t
[@@deriving show]

let rec remove_paren = function
    | Paren e -> remove_paren e
    | Neg e -> Neg (remove_paren e)
    | Int i -> Int i
    | Add (lhr, rhr) -> Add (remove_paren lhr, remove_paren rhr)
    | Sub (lhr, rhr) -> Sub (remove_paren lhr, remove_paren rhr)
    | Mul (lhr, rhr) -> Mul (remove_paren lhr, remove_paren rhr)
    | Div (lhr, rhr) -> Div (remove_paren lhr, remove_paren rhr)
    | Mod (lhr, rhr) -> Mod (remove_paren lhr, remove_paren rhr)
