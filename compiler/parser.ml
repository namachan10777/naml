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

exception SyntaxError

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

let rec parse input =
    match (split_with is_add_or_sub @@ List.rev input, input) with
    | (Some ([Int rhr], lhr), _) ->
        Ast.Add (parse (List.rev lhr), Ast.Int rhr)
    | (None, [Int i]) ->
        Ast.Int i
    | _ -> raise SyntaxError

let () =
    Test.assert_eq "parse_add" (parse [Int 0; Add; Int 1; Add; Int 2])
    (Ast.Add (Ast.Add(Ast.Int 0, Ast.Int 1), Ast.Int 2));
