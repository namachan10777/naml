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

let rec take_params = function
    | Ident id :: remain ->
        let (params, remain) = take_params remain in
        (id :: params, remain)
    | remain -> ([], remain)

type param_taken_t = string list * input_t
[@@deriving show]

let rec parse_expr = function
    | Let :: Ident id :: Eq :: remain -> begin match parse_expr remain with
        | (def, In :: remain) ->
            let (expr, remain) = parse_expr remain in (Ast.Let (id, def, expr), remain)
        | _ -> raise @@ SyntaxError "let"
    end
    | Fun :: (Ident _ :: _ as remain) -> begin match take_params remain with
        | ((_ :: _) as params, Arrow :: expr) ->
            let (expr, remain) = parse_expr expr in
            (Ast.Fun (params, expr), remain)
        | x -> raise @@ SyntaxError (Printf.sprintf "fun %s <---" @@ show_param_taken_t x)
    end
    | others -> parse_or others
and parse_or input = match parse_and input with
    | (lhr, Or :: (Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Or (lhr, rhr), remain)
    | (lhr, Or :: rhr) -> begin match parse_or rhr with
        | (Ast.Or (rhrl, rhrr), remain) -> (Ast.Or (Ast.Or (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.Or (lhr, rhr), remain)
    end
    | x -> x
and parse_and input = match parse_eq input with
    | (lhr, And :: (Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.And (lhr, rhr), remain)
    | (lhr, And :: (Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.And (lhr, rhr), remain)
    | (lhr, And :: rhr) -> begin match parse_and rhr with
        | (Ast.And (rhrl, rhrr), remain) -> (Ast.And (Ast.And (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.And (lhr, rhr), remain)
    end
    | x -> x
and parse_eq input = match parse_cons input with
    | (lhr, Eq :: (Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Eq (lhr, rhr), remain)
    | (lhr, Eq :: (Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Eq (lhr, rhr), remain)
    | (lhr, Eq :: rhr) -> begin match parse_eq rhr with
        | (Ast.Eq (rhrl, rhrr), remain) -> (Ast.Eq (Ast.Eq (lhr, rhrl), rhrr), remain)
        | (Ast.Neq (rhrl, rhrr), remain) -> (Ast.Neq (Ast.Eq (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.Eq (lhr, rhr), remain)
    end
    | (lhr, Neq :: (Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Neq (lhr, rhr), remain)
    | (lhr, Neq :: (Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Neq (lhr, rhr), remain)
    | (lhr, Neq :: rhr) -> begin match parse_eq rhr with
        | (Ast.Eq (rhrl, rhrr), remain) -> (Ast.Eq (Ast.Neq (lhr, rhrl), rhrr), remain)
        | (Ast.Neq (rhrl, rhrr), remain) -> (Ast.Neq (Ast.Neq (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.Neq (lhr, rhr), remain)
    end
    | (lhr, Gret :: (Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Gret (lhr, rhr), remain)
    | (lhr, Gret :: (Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Gret (lhr, rhr), remain)
    | (lhr, Gret :: rhr) ->
        let (rhr, remain) = parse_cons rhr in
        (Ast.Gret (lhr, rhr), remain)
    | (lhr, Less :: (Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Less (lhr, rhr), remain)
    | (lhr, Less :: (Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Less (lhr, rhr), remain)
    | (lhr, Less :: rhr) ->
        let (rhr, remain) = parse_cons rhr in
        (Ast.Less (lhr, rhr), remain)
    | x -> x
and parse_cons input = match parse_add input with
    | (lhr, Cons :: (Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Cons (lhr, rhr), remain)
    | (lhr, Cons :: (Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Cons (lhr, rhr), remain)
    | (lhr, Cons :: rhr) ->
        let (rhr, remain) = parse_cons rhr in
        Ast.Cons (lhr, rhr), remain
    | x -> x
and parse_add input = match parse_mul input with | (lhr, Add :: (Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Add (lhr, rhr), remain)
    | (lhr, Add :: (Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Add (lhr, rhr), remain)
    | (lhr, Add :: rhr) -> begin match parse_add rhr with
        | (Ast.Add (rhrl, rhrr), remain) -> (Ast.Add (Ast.Add (lhr, rhrl), rhrr), remain)
        | (Ast.Sub (rhrl, rhrr), remain) -> (Ast.Sub (Ast.Add (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.Add (lhr, rhr), remain)
    end
    | (lhr, Sub :: (Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Sub (lhr, rhr), remain)
    | (lhr, Sub :: (Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Sub (lhr, rhr), remain)
    | (lhr, Sub :: rhr) -> begin match parse_add rhr with
        | (Ast.Add (rhrl, rhrr), remain) -> (Ast.Add (Ast.Sub (lhr, rhrl), rhrr), remain)
        | (Ast.Sub (rhrl, rhrr), remain) -> (Ast.Sub (Ast.Sub (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.Sub (lhr, rhr), remain)
    end
    | x -> x
and parse_mul input = match parse_term input with
    | (lhr, Mul :: (Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Mul (lhr, rhr), remain)
    | (lhr, Mul :: (Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Mul (lhr, rhr), remain)
    | (lhr, Mul :: rhr) -> begin match parse_mul rhr with
        | (Ast.Mul (rhrl, rhrr), remain) -> (Ast.Mul (Ast.Mul (lhr, rhrl), rhrr), remain)
        | (Ast.Div (rhrl, rhrr), remain) -> (Ast.Div (Ast.Mul (lhr, rhrl), rhrr), remain)
        | (Ast.Mod (rhrl, rhrr), remain) -> (Ast.Mod (Ast.Mul (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.Mul (lhr, rhr), remain)
    end
    | (lhr, Div :: (Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Div (lhr, rhr), remain)
    | (lhr, Div :: (Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Div (lhr, rhr), remain)
    | (lhr, Div :: rhr) -> begin match parse_mul rhr with
        | (Ast.Mul (rhrl, rhrr), remain) -> (Ast.Mul (Ast.Div (lhr, rhrl), rhrr), remain)
        | (Ast.Div (rhrl, rhrr), remain) -> (Ast.Div (Ast.Div (lhr, rhrl), rhrr), remain)
        | (Ast.Mod (rhrl, rhrr), remain) -> (Ast.Mod (Ast.Div (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Ast.Div (lhr, rhr), remain)
    end
    | (lhr, Mod :: (Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Mod (lhr, rhr), remain)
    | (lhr, Mod :: (Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Ast.Mod (lhr, rhr), remain)
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
    | Sub :: (Let :: _ as remain) ->
        let (exp, remain) = parse_expr remain in
        (Ast.Neg exp ,remain)
    | Sub :: (Fun :: _ as remain) ->
        let (exp, remain) = parse_expr remain in
        (Ast.Neg exp ,remain)
    | Sub :: remain ->
        let (exp, remain) = parse_term remain in
        (Ast.Neg exp ,remain)
    | Not :: (Let :: _ as remain) ->
        let (exp, remain) = parse_expr remain in
        (Ast.Not exp ,remain)
    | Not :: (Fun :: _ as remain) ->
        let (exp, remain) = parse_expr remain in
        (Ast.Not exp ,remain)
    | Not :: remain ->
        let (exp, remain) = parse_term remain in
        (Ast.Not exp ,remain)
    | LB :: RB :: remain -> (Ast.Emp, remain)
    | LP :: remain -> begin match parse_expr remain with
        | (inner, RP :: remain) -> (Ast.Paren inner, remain)
        | x -> raise @@ SyntaxError (Printf.sprintf "paren is not balanced %s " @@ show_parsed_t x)
    end
    | x -> (dbg_i x); raise @@ SyntaxError "term"

let parse input = match parse_expr input with
    | (ast, [Eof]) -> Ast.remove_paren ast
    | x -> (dbg x); raise @@ SyntaxError "top"
