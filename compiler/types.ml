type ty_t =
    | Int
    | Str
    | Bool
    | Arrow of ty_t * ty_t
    | List of ty_t
    | Tuple of ty_t list
    | Array of ty_t
    | Unknown of int
    | Forall of int
    | Higher of int
    | Ref of ty_t
[@@deriving show]

let pervasive_vals = [
    "+", Arrow (Int, Arrow (Int, Int));
    "-", Arrow (Int, Arrow (Int, Int));
    "*", Arrow (Int, Arrow (Int, Int));
    "/", Arrow (Int, Arrow (Int, Int));
    "mod", Arrow (Int, Arrow (Int, Int));
    "=", Arrow (Forall 0, Arrow (Forall 0, Bool));
    "<>", Arrow (Forall 0, Arrow (Forall 0, Bool));
    ">", Arrow (Int, Arrow (Int, Bool));
    "<", Arrow (Int, Arrow (Int, Bool));
    ";", Arrow (Forall 0, Arrow (Forall 1, Bool));
    "::", Arrow (Forall 0, Arrow (List (Forall 1), Bool));
    ".", Arrow (Array (Forall 0), (Arrow (Int, Forall 0)));
    "<unary>", Arrow (Int, Int);
    "<not>", Arrow (Bool, Bool);
    "<ref>", Arrow (Forall 0, Ref (Forall 0));
    ":=", Arrow (Ref (Forall 0), Arrow (Forall 0, Tuple []));
]

let pervasive_types = [
    "int", Int;
    "bool", Bool;
    "option", Higher 0;
]

let pervasive_ctors = [
    "Some", Arrow (Forall 0, Higher 0);
    "None", Higher 0;
]

let ids = List.mapi (fun i (id, _) -> ([id], i))
let types = List.mapi (fun i (_, ty) -> (i, ty))
let pervasive_val_ids = ids pervasive_vals
let pervasive_val_types = types pervasive_vals
let pervasive_type_ids = ids pervasive_types
let pervasive_type_types = types pervasive_types
let pervasive_ctor_ids = ids pervasive_ctors
let pervasive_ctor_types = types pervasive_ctors
