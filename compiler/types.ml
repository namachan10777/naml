type ty_t =
    | Int
    | Str
    | Bool
    | Arrow of ty_t * ty_t
    | List of ty_t
    | Tuple of ty_t list
    | Array of ty_t
    | Var of int
    | Ref of ty_t
[@@deriving show]

let builtin_def = [
    "+", Arrow (Int, Arrow (Int, Int));
    "-", Arrow (Int, Arrow (Int, Int));
    "*", Arrow (Int, Arrow (Int, Int));
    "/", Arrow (Int, Arrow (Int, Int));
    "mod", Arrow (Int, Arrow (Int, Int));
    "=", Arrow (Var 0, Arrow (Var 0, Bool));
    "<>", Arrow (Var 0, Arrow (Var 0, Bool));
    ">", Arrow (Int, Arrow (Int, Bool));
    "<", Arrow (Int, Arrow (Int, Bool));
    ";", Arrow (Var 0, Arrow (Var 1, Bool));
    "::", Arrow (Var 0, Arrow (List (Var 1), Bool));
    ".", Arrow (Array (Var 0), (Arrow (Int, Var 0)));
    "<unary>", Arrow (Int, Int);
    "<not>", Arrow (Bool, Bool);
    "<ref>", Arrow (Var 0, Ref (Var 0));
    ":=", Arrow (Ref (Var 0), Arrow (Var 0, Tuple []));
]

let initial_alpha_env =
    List.mapi (fun i (x, _) -> ([x], i)) builtin_def

let initial_type_env =
    List.mapi (fun i (_, ty) -> (i, ty)) builtin_def
