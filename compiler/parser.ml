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

type parsed_t = Ast.t * token_t list
[@@deriving show]

exception SyntaxError of string

let rec split_with f = function
    | hd :: tl ->
        if f hd
        then Some ([], tl)
        else begin match split_with f tl with
        | Some(first_half, second_half) -> Some(hd :: first_half, second_half)
        | None -> None
        end
    | [] -> None

let is_add_or_sub = function
    | Add -> true
    | Sub -> true
    | _ -> false

let dbg ast = print_endline @@ show_parsed_t ast
let dbg_i ast = print_endline @@ show_input_t ast

let rec parse_expr input = match parse_term input with
    | (lhr, Add :: rhr) -> begin match parse_expr rhr with
        | (Ast.Add (rhrl, rhrr), remain) -> (Ast.Add(Ast.Add (lhr, rhrl), rhrr), remain)
        | (Ast.Sub (rhrl, rhrr), remain) -> (Ast.Add(Ast.Sub (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.Add (lhr, rhr), remain)
    end
    | x -> x
and parse_term = function
    | Int i :: remain -> (Ast.Int i, remain)
    | x -> (dbg_i x); raise @@ SyntaxError "term"

let parse input = match parse_expr input with
    | (ast, [Eof]) -> ast
    | x -> (dbg x); raise @@ SyntaxError "top"
