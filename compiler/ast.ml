type t =
    | Emp
    | Int of int
    | Bool of bool
    | Var of string
    | Tuple of t list
    | If of t * t * t
    | Let of string * t * t
    | Fun of string list * t
    | App of t * t
[@@deriving show]


let rec of_parser_t = function
    | Parser.Int i -> Int i
    | Parser.Bool i -> Bool i
    | Parser.Var i -> Var i
    | Parser.Emp -> Emp
    | Parser.Add (lhr, rhr) -> op "+" lhr rhr
    | Parser.Sub (lhr, rhr) -> op "-" lhr rhr
    | Parser.Mul (lhr, rhr) -> op "*" lhr rhr
    | Parser.Div (lhr, rhr) -> op "/" lhr rhr
    | Parser.Mod (lhr, rhr) -> op "mod" lhr rhr
    | Parser.Or (lhr, rhr) -> If (of_parser_t lhr, Bool true, of_parser_t rhr)
    | Parser.And (lhr, rhr) -> If (of_parser_t lhr, of_parser_t rhr, Bool false)
    | Parser.Eq (lhr, rhr) -> op "=" lhr rhr
    | Parser.Neq (lhr, rhr) -> op "<>" lhr rhr
    | Parser.Seq (lhr, rhr) -> op ";" lhr rhr
    | Parser.Cons (lhr, rhr) -> op "::" lhr rhr
    | Parser.Gret (lhr, rhr) -> op ">" lhr rhr
    | Parser.Less (lhr, rhr) -> op "<" lhr rhr
    | Parser.Neg e -> App (Var "\\unary", of_parser_t e)
    | Parser.Not e -> App (Var "\\not", of_parser_t e)
    | Parser.Pipeline (arg, f) -> App(of_parser_t f, of_parser_t arg)
    | Parser.Tuple elem -> Tuple (List.map of_parser_t elem)
    | Parser.If (cond, e1, e2) -> If (of_parser_t cond, of_parser_t e1, of_parser_t e2)
    | Parser.Let (id, def, expr) -> Let (id, of_parser_t def, of_parser_t expr)
    | Parser.Fun (params, expr) -> Fun (params, of_parser_t expr)
    | Parser.App (f, arg) -> App (of_parser_t f, of_parser_t arg)
    | Parser.Paren e -> of_parser_t e
and op id lhr rhr = App (App (Var id, of_parser_t lhr), of_parser_t rhr)

