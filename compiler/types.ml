type t =
    | Int
    | Str
    | Bool
    | Arrow of t ref * t ref
    | List of t ref
    | Tuple of t ref list
    | Array of t ref
    (* level * id *)
    | Unknown of int * int
    | Poly of int
    | Higher of t ref * string list
    | Ref of t ref
[@@deriving show]

let pervasive_vals = [
    ["+"], Arrow (ref Int, ref (Arrow (ref Int, ref Int)));
    ["-"], Arrow (ref Int, ref (Arrow (ref Int, ref Int)));
    ["*"], Arrow (ref Int, ref (Arrow (ref Int, ref Int)));
    ["/"], Arrow (ref Int, ref (Arrow (ref Int, ref Int)));
    ["mod"], Arrow (ref Int, ref (Arrow (ref Int, ref Int)));
    ["="], Arrow (ref (Poly 0), ref (Arrow (ref (Poly 0), ref Bool)));
    ["<>"], Arrow (ref (Poly 0), ref (Arrow (ref (Poly 0), ref Bool)));
    [">"], Arrow (ref Int, ref (Arrow (ref Int, ref Bool)));
    ["<"], Arrow (ref Int, ref (Arrow (ref Int, ref Bool)));
    [";"], Arrow (ref (Poly 0), ref (Arrow (ref (Poly 1), ref Bool)));
    ["::"], Arrow (ref (Poly 0), ref (Arrow (ref (List (ref (Poly 1))), ref Bool)));
    ["."], Arrow (ref (Array (ref (Poly 0))), ref (Arrow (ref Int, ref (Poly 0))));
    ["<unary>"], Arrow (ref Int, ref Int);
    ["not"], Arrow (ref Bool, ref Bool);
    ["ref"], Arrow (ref (Poly 0), ref (Ref (ref (Poly 0))));
    [":="], Arrow (ref (Ref (ref (Poly 0))), ref (Poly 0));
]

let pervasive_types = [
    ["int"], Int;
    ["bool"], Bool;
    ["option"], Higher (ref (Poly 0), ["option"]);
]

let pervasive_ctors = [
    ["Some"], Arrow (ref (Poly 0), ref (Higher (ref (Poly 0), ["option"])));
    ["None"], Higher (ref (Poly 0), ["option"]);
]

let ids = List.mapi (fun i (id, _) -> (id, i))
let types = List.mapi (fun i (_, ty) -> (i, ty))
let pervasive_val_ids = ids pervasive_vals
let pervasive_val_types = types pervasive_vals
let pervasive_type_ids = ids pervasive_types
let pervasive_type_types = types pervasive_types
let pervasive_ctor_ids = ids pervasive_ctors
let pervasive_ctor_types = types pervasive_ctors
