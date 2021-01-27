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

type input_t = Lex.t list
[@@deriving show]

type parsed_t = t * Lex.t list
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

let dbg ast = print_endline @@ show_parsed_t ast
let dbg_i ast = print_endline @@ show_input_t ast

let rec take_params = function
    | Lex.Ident id :: remain ->
        let (params, remain) = take_params remain in
        (id :: params, remain)
    | remain -> ([], remain)

type param_taken_t = string list * input_t
[@@deriving show]

let rec parse_expr = function
    | Lex.Let :: Ident id :: Eq :: remain -> begin match parse_expr remain with
        | (def, Lex.In :: remain) ->
            let (expr, remain) = parse_expr remain in (Let (id, def, expr), remain)
        | _ -> raise @@ SyntaxError "let"
    end
    | Lex.Fun :: (Ident _ :: _ as remain) -> begin match take_params remain with
        | ((_ :: _) as params, Lex.Arrow :: expr) ->
            let (expr, remain) = parse_expr expr in
            (Fun (params, expr), remain)
        | x -> raise @@ SyntaxError (Printf.sprintf "fun %s <---" @@ show_param_taken_t x)
    end
    | others -> parse_seq others
and parse_seq input = match parse_pipeline input with
    | (lhr, Lex.Semicol :: (Lex.Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_pipeline rhr in (Seq (lhr, rhr), remain)
    | (lhr, Lex.Semicol :: rhr) ->
        let (rhr, remain) = parse_seq rhr in (Seq (lhr, rhr), remain)
    | x -> x
and parse_pipeline input = match parse_atat input with
    | (lhr, Lex.Pipeline :: (Lex.Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Pipeline (lhr, rhr), remain)
    | (lhr, Lex.Pipeline :: rhr) -> begin match parse_pipeline rhr with
        | (Pipeline (rhrl, rhrr), remain) -> (Pipeline (Pipeline (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Pipeline (lhr, rhr), remain)
    end
    | x -> x
and parse_atat input = match parse_or input with
    | (lhr, Lex.AtAt :: (Lex.Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (App (lhr, rhr), remain)
    | (lhr, Lex.AtAt :: rhr) ->
        let (rhr, remain) = parse_atat rhr in (App (lhr, rhr), remain)
    | x -> x
and parse_or input = match parse_and input with
    | (lhr, Lex.Or :: (Lex.Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Or (lhr, rhr), remain)
    | (lhr, Lex.Or :: rhr) -> begin match parse_or rhr with
        | (Or (rhrl, rhrr), remain) -> (Or (Or (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Or (lhr, rhr), remain)
    end
    | x -> x
and parse_and input = match parse_eq input with
    | (lhr, Lex.And :: (Lex.Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (And (lhr, rhr), remain)
    | (lhr, Lex.And :: (Lex.Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (And (lhr, rhr), remain)
    | (lhr, Lex.And :: rhr) -> begin match parse_and rhr with
        | (And (rhrl, rhrr), remain) -> (And (And (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (And (lhr, rhr), remain)
    end
    | x -> x
and parse_eq input = match parse_cons input with
    | (lhr, Lex.Eq :: (Lex.Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Eq (lhr, rhr), remain)
    | (lhr, Lex.Eq :: (Lex.Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Eq (lhr, rhr), remain)
    | (lhr, Lex.Eq :: rhr) -> begin match parse_eq rhr with
        | (Eq (rhrl, rhrr), remain) -> (Eq (Eq (lhr, rhrl), rhrr), remain)
        | (Neq (rhrl, rhrr), remain) -> (Neq (Eq (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Eq (lhr, rhr), remain)
    end
    | (lhr, Lex.Neq :: (Lex.Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Neq (lhr, rhr), remain)
    | (lhr, Lex.Neq :: (Lex.Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Neq (lhr, rhr), remain)
    | (lhr, Lex.Neq :: rhr) -> begin match parse_eq rhr with
        | (Eq (rhrl, rhrr), remain) -> (Eq (Neq (lhr, rhrl), rhrr), remain)
        | (Neq (rhrl, rhrr), remain) -> (Neq (Neq (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Neq (lhr, rhr), remain)
    end
    | (lhr, Lex.Gret :: (Lex.Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Gret (lhr, rhr), remain)
    | (lhr, Lex.Gret :: (Lex.Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Gret (lhr, rhr), remain)
    | (lhr, Lex.Gret :: rhr) ->
        let (rhr, remain) = parse_cons rhr in
        (Gret (lhr, rhr), remain)
    | (lhr, Lex.Less :: (Lex.Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Less (lhr, rhr), remain)
    | (lhr, Lex.Less :: (Lex.Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Less (lhr, rhr), remain)
    | (lhr, Lex.Less :: rhr) ->
        let (rhr, remain) = parse_cons rhr in
        (Less (lhr, rhr), remain)
    | x -> x
and parse_cons input = match parse_add input with
    | (lhr, Lex.Cons :: (Lex.Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Cons (lhr, rhr), remain)
    | (lhr, Lex.Cons :: (Lex.Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Cons (lhr, rhr), remain)
    | (lhr, Lex.Cons :: rhr) ->
        let (rhr, remain) = parse_cons rhr in
        Cons (lhr, rhr), remain
    | x -> x
and parse_add input = match parse_mul input with
    | (lhr, Lex.Add :: (Lex.Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Add (lhr, rhr), remain)
    | (lhr, Lex.Add :: (Lex.Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Add (lhr, rhr), remain)
    | (lhr, Lex.Add :: rhr) -> begin match parse_add rhr with
        | (Add (rhrl, rhrr), remain) -> (Add (Add (lhr, rhrl), rhrr), remain)
        | (Sub (rhrl, rhrr), remain) -> (Sub (Add (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Add (lhr, rhr), remain)
    end
    | (lhr, Lex.Sub :: (Lex.Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Sub (lhr, rhr), remain)
    | (lhr, Lex.Sub :: (Lex.Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Sub (lhr, rhr), remain)
    | (lhr, Lex.Sub :: rhr) -> begin match parse_add rhr with
        | (Add (rhrl, rhrr), remain) -> (Add (Sub (lhr, rhrl), rhrr), remain)
        | (Sub (rhrl, rhrr), remain) -> (Sub (Sub (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Sub (lhr, rhr), remain)
    end
    | x -> x
and parse_mul input = match parse_term input with
    | (lhr, Lex.Mul :: (Lex.Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Mul (lhr, rhr), remain)
    | (lhr, Lex.Mul :: (Lex.Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Mul (lhr, rhr), remain)
    | (lhr, Lex.Mul :: rhr) -> begin match parse_mul rhr with
        | (Mul (rhrl, rhrr), remain) -> (Mul (Mul (lhr, rhrl), rhrr), remain)
        | (Div (rhrl, rhrr), remain) -> (Div (Mul (lhr, rhrl), rhrr), remain)
        | (Mod (rhrl, rhrr), remain) -> (Mod (Mul (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Mul (lhr, rhr), remain)
    end
    | (lhr, Lex.Div :: (Lex.Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Div (lhr, rhr), remain)
    | (lhr, Lex.Div :: (Lex.Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Div (lhr, rhr), remain)
    | (lhr, Lex.Div :: rhr) -> begin match parse_mul rhr with
        | (Mul (rhrl, rhrr), remain) -> (Mul (Div (lhr, rhrl), rhrr), remain)
        | (Div (rhrl, rhrr), remain) -> (Div (Div (lhr, rhrl), rhrr), remain)
        | (Mod (rhrl, rhrr), remain) -> (Mod (Div (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Div (lhr, rhr), remain)
    end
    | (lhr, Lex.Mod :: (Lex.Fun :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Mod (lhr, rhr), remain)
    | (lhr, Lex.Mod :: (Lex.Let :: _ as rhr)) ->
        let (rhr, remain ) = parse_expr rhr in (Mod (lhr, rhr), remain)
    | (lhr, Lex.Mod :: rhr) -> begin match parse_mul rhr with
        | (Mul (rhrl, rhrr), remain) -> (Mul (Mod (lhr, rhrl), rhrr), remain)
        | (Div (rhrl, rhrr), remain) -> (Div (Mod (lhr, rhrl), rhrr), remain)
        | (Mod (rhrl, rhrr), remain) -> (Mod (Mod (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Mod (lhr, rhr), remain)
    end
    | x -> x
and parse_term = function
    | Lex.Ident id :: remain -> (Var id, remain)
    | Lex.Int i :: remain -> (Int i, remain)
    | Lex.True :: remain -> (Bool true, remain)
    | Lex.False :: remain -> (Bool false, remain)
    | Lex.Sub :: (Lex.Let :: _ as remain) ->
        let (exp, remain) = parse_expr remain in
        (Neg exp ,remain)
    | Lex.Sub :: (Lex.Fun :: _ as remain) ->
        let (exp, remain) = parse_expr remain in
        (Neg exp ,remain)
    | Lex.Sub :: remain ->
        let (exp, remain) = parse_term remain in
        (Neg exp ,remain)
    | Lex.Not :: (Lex.Let :: _ as remain) ->
        let (exp, remain) = parse_expr remain in
        (Not exp ,remain)
    | Lex.Not :: (Lex.Fun :: _ as remain) ->
        let (exp, remain) = parse_expr remain in
        (Not exp ,remain)
    | Lex.Not :: remain ->
        let (exp, remain) = parse_term remain in
        (Not exp ,remain)
    | Lex.LB :: Lex.RB :: remain -> (Emp, remain)
    | Lex.LP :: remain -> begin match parse_expr remain with
        | (inner, RP :: remain) -> (Paren inner, remain)
        | x -> raise @@ SyntaxError (Printf.sprintf "paren is not balanced %s " @@ show_parsed_t x)
    end
    | x -> (dbg_i x); raise @@ SyntaxError "term"

let parse input = match parse_expr input with
    | (ast, [Lex.Eof]) -> ast
    | x -> (dbg x); raise @@ SyntaxError "top"
