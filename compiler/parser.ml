type token_t =
    | Str of string
    | Int of int
    | Ident of string
    | True
    | False
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Cons
    | Gret
    | Less
    | Eq
    | Neq
    | And
    | Or
    | LP
    | RP
    | LB
    | RB
    | Semicol
    | Comma
    | VBar
    | Arrow
    | Fun
    | If
    | Then
    | Else
    | Let
    | In
    | Rec
    | Match
    | With
    | Builtin
    | Not
    | Ref
    | Eof
[@@deriving show]

type input_t = token_t list
[@@deriving show]
