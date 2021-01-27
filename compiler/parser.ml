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

let rec parse_expr input = parse_or input
and parse_or input = match parse_and input with
    | (lhr, Or :: rhr) -> begin match parse_or rhr with
        | (Ast.Or (rhrl, rhrr), remain) -> (Ast.Or (Ast.Or (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.Or (lhr, rhr), remain)
    end
    | x -> x
and parse_and input = match parse_eq input with
    | (lhr, And :: rhr) -> begin match parse_and rhr with
        | (Ast.And (rhrl, rhrr), remain) -> (Ast.And (Ast.And (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.And (lhr, rhr), remain)
    end
    | x -> x
and parse_eq input = match parse_add input with
    | (lhr, Eq :: rhr) -> begin match parse_eq rhr with
        | (Ast.Eq (rhrl, rhrr), remain) -> (Ast.Eq (Ast.Eq (lhr, rhrl), rhrr), remain)
        | (Ast.Neq (rhrl, rhrr), remain) -> (Ast.Neq (Ast.Eq (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.Eq (lhr, rhr), remain)
    end
    | (lhr, Neq :: rhr) -> begin match parse_eq rhr with
        | (Ast.Eq (rhrl, rhrr), remain) -> (Ast.Eq (Ast.Neq (lhr, rhrl), rhrr), remain)
        | (Ast.Neq (rhrl, rhrr), remain) -> (Ast.Neq (Ast.Neq (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.Neq (lhr, rhr), remain)
    end
    | (lhr, Gret :: rhr) ->
        let (rhr, remain) = parse_add rhr in
        (Ast.Gret (lhr, rhr), remain)
    | (lhr, Less :: rhr) ->
        let (rhr, remain) = parse_add rhr in
        (Ast.Less (lhr, rhr), remain)
    | x -> x
and parse_add input = match parse_mul input with
    | (lhr, Add :: rhr) -> begin match parse_add rhr with
        | (Ast.Add (rhrl, rhrr), remain) -> (Ast.Add (Ast.Add (lhr, rhrl), rhrr), remain)
        | (Ast.Sub (rhrl, rhrr), remain) -> (Ast.Sub (Ast.Add (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.Add (lhr, rhr), remain)
    end
    | (lhr, Sub :: rhr) -> begin match parse_add rhr with
        | (Ast.Add (rhrl, rhrr), remain) -> (Ast.Add (Ast.Sub (lhr, rhrl), rhrr), remain)
        | (Ast.Sub (rhrl, rhrr), remain) -> (Ast.Sub (Ast.Sub (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.Sub (lhr, rhr), remain)
    end
    | x -> x
and parse_mul input = match parse_term input with
    | (lhr, Mul :: rhr) -> begin match parse_mul rhr with
        | (Ast.Mul (rhrl, rhrr), remain) -> (Ast.Mul (Ast.Mul (lhr, rhrl), rhrr), remain)
        | (Ast.Div (rhrl, rhrr), remain) -> (Ast.Div (Ast.Mul (lhr, rhrl), rhrr), remain)
        | (Ast.Mod (rhrl, rhrr), remain) -> (Ast.Mod (Ast.Mul (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.Mul (lhr, rhr), remain)
    end
    | (lhr, Div :: rhr) -> begin match parse_mul rhr with
        | (Ast.Mul (rhrl, rhrr), remain) -> (Ast.Mul (Ast.Div (lhr, rhrl), rhrr), remain)
        | (Ast.Div (rhrl, rhrr), remain) -> (Ast.Div (Ast.Div (lhr, rhrl), rhrr), remain)
        | (Ast.Mod (rhrl, rhrr), remain) -> (Ast.Mod (Ast.Div (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.Div (lhr, rhr), remain)
    end
    | (lhr, Mod :: rhr) -> begin match parse_mul rhr with
        | (Ast.Mul (rhrl, rhrr), remain) -> (Ast.Mul (Ast.Mod (lhr, rhrl), rhrr), remain)
        | (Ast.Div (rhrl, rhrr), remain) -> (Ast.Div (Ast.Mod (lhr, rhrl), rhrr), remain)
        | (Ast.Mod (rhrl, rhrr), remain) -> (Ast.Mod (Ast.Mod (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.Mod (lhr, rhr), remain)
    end
    | x -> x
and parse_term = function
    | Ident id :: remain -> (Ast.Var id, remain)
    | Int i :: remain -> (Ast.Int i, remain)
    | True :: remain -> (Ast.Bool true, remain)
    | False :: remain -> (Ast.Bool false, remain)
    | Sub :: remain ->
        let (exp, remain) = parse_term remain in
        (Ast.Neg exp ,remain)
    | Not :: remain ->
        let (exp, remain) = parse_term remain in
        (Ast.Not exp ,remain)
    | LP :: remain -> begin match parse_expr remain with
        | (inner, RP :: remain) -> (Ast.Paren inner, remain)
        | x -> raise @@ SyntaxError (Printf.sprintf "paren is not balanced %s " @@ show_parsed_t x)
    end
    | x -> (dbg_i x); raise @@ SyntaxError "term"

let parse input = match parse_expr input with
    | (ast, [Eof]) -> Ast.remove_paren ast
    | x -> (dbg x); raise @@ SyntaxError "top"
