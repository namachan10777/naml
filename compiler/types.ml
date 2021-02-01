type addresses_t = int list
[@@deriving show]

type t =
    | Int
    | Str
    | Bool
    | Fun of t list * t
    | Tuple of t list
    (* 不明の型。最初のintはlevelで2つめのintはデバッグ用のタグ *)
    | Unknown of unknown_t ref ref
    (* 多相型。intはタグ *)
    | Poly of int
    | Variant of string list
    | Higher of t * string list
and unknown_t = U of int * int * unk_payload_t * unknown_t ref list
[@printer fun fmt (level, tag, t, refs) ->
    fprintf fmt "(%d, %d, %s, %s)" level tag (show_unk_payload_t t) (refs |> List.map (fun r -> 2 * Obj.magic r) |> show_addresses_t)
]
and unk_payload_t = t option
[@@deriving show]

let rec eq a b = match a, b with
    | Unknown u, Unknown u' ->
        let U (level, tag, t, refs) = ! !u in
        let U (level', tag', t', refs') = ! !u' in
        let addr_conv = List.map (fun r -> 2 * Obj.magic r) in
        level = level' &&
        tag = tag' &&
        t = t' &&
        (addr_conv refs) = (addr_conv refs')
    | Int, Int -> true
    | Str, Str -> true
    | Bool, Bool -> true
    | Fun (args, ret), Fun (args', ret') ->
        (Util.zip args args' |> List.for_all (fun (a, b) -> eq a b))
        && eq ret ret'
    | Tuple ts, Tuple ts' ->
        Util.zip ts ts' |> List.for_all (fun (a, b) -> eq a b)
    | Poly t, Poly t' -> t = t'
    | Higher (t, name), Higher (t', name') -> (eq t t') && name = name'
    | _ -> false

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
    ["::"], Fun ([Poly 0; Higher (Poly 0, ["list"])], Higher (Poly 0, ["list"]));
    ["."], Fun ([Higher (Poly 0, ["array"]); Int], Poly 0);
    ["<neg>"], Fun ([Int], Int);
    ["not"], Fun ([Bool], Bool);
    ["ref"], Fun ([Poly 0], Higher (Poly 0, ["ref"]));
    [":="], Fun ([Higher (Poly 0, ["ref"]); Poly 0], unit_ty);
    ["[]"], Higher (Poly 0, ["list"]);
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
