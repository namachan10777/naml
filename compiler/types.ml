type t =
    | Int
    | Str
    | Bool
    | Fun of t list * t
    | Tuple of t list
    | Array of t
    (* 不明の型。最初のintはlevelで2つめのintはデバッグ用のタグ *)
    | Unknown of int * (int * t option) ref ref
    (* 多相型。intはタグ *)
    | Poly of int
    | Higher of t * string list
    | Ref of t
[@@deriving show]

let unit_ty = Tuple []

let pervasive_vals = [
    ["+"], Fun ([Int; Int], Int);
    ["-"], Fun ([Int; Int], Int);
    ["*"], Fun ([Int; Int], Int);
    ["/"], Fun ([Int; Int], Int);
    ["mod"], Fun ([Int; Int], Int);
    [">"], Fun ([Int; Int], Bool);
    ["<"], Fun ([Int; Int], Bool);
    ["="], Fun ([Poly 0; Poly 0], Bool);
    [";"], Fun ([Poly 0], Poly 1);
    ["::"], Fun ([Poly 0], Higher (Poly 0, ["list"]));
    ["."], Fun ([Higher (Poly 0, ["array"]); Int], Poly 0);
    ["<neg>"], Fun ([Int], Int);
    ["not"], Fun ([Bool], Bool);
    ["ref"], Fun ([Poly 0], Ref (Poly 0));
    [":="], Fun ([Ref (Poly 0); Poly 0], unit_ty);
]

let pervasive_types = [
    ["int"], Int;
    ["bool"], Bool;
    ["option"], Higher (Poly 0, ["option"]);
]

let pervasive_ctors = [
    ["Some"], Fun ([Poly 0], Higher (Poly 0, ["option"]));
    ["None"], Higher (Poly 0, ["option"]);
]

let ids = List.mapi (fun i (id, _) -> (id, i))
let types = List.mapi (fun i (_, ty) -> (i, ty))
let pervasive_val_ids = ids pervasive_vals
let pervasive_val_types = types pervasive_vals
let pervasive_type_ids = ids pervasive_types
let pervasive_type_types = types pervasive_types
let pervasive_ctor_ids = ids pervasive_ctors
let pervasive_ctor_types = types pervasive_ctors
