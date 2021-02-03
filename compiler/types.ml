type addresses_t = int list [@@deriving show]

type t =
    | Int
    | Str
    | Bool
    | Fun of t list * t
    | Tuple of t list
    (* 不明の型。最初のintはlevelで2つめのintはデバッグ用のタグ *)
    | Var of var_t ref ref
    (* 多相型。intはタグ *)
    | Poly of int
    | Variant of t list * string list
[@@deriving show]

and var_t = Unknown of int * int * var_t ref list | Just of t * var_t ref list
[@@printer
    fun fmt -> function
      | Unknown (level, tag, refs) -> fprintf fmt "Unknown (%d, %d)" level tag
      | Just (t, refs) -> fprintf fmt "Just (%s)" (show t)]

let rec eq a b =
    match (a, b) with
    | Var v, Var v' -> (
      match (!(!v), !(!v')) with
      | Unknown (level, tag, refs), Unknown (level', tag', refs') ->
          let addr_conv = List.map (fun r -> 2 * Obj.magic r) in
          level = level' && tag = tag' && addr_conv refs = addr_conv refs'
      | Just (t, refs), Just (t', refs') ->
          let addr_conv = List.map (fun r -> 2 * Obj.magic r) in
          addr_conv refs = addr_conv refs' && eq t t'
      | _ -> false )
    | Int, Int -> true
    | Str, Str -> true
    | Bool, Bool -> true
    | Fun (args, ret), Fun (args', ret') ->
        Util.zip args args' |> List.for_all (fun (a, b) -> eq a b)
        && eq ret ret'
    | Tuple ts, Tuple ts' ->
        Util.zip ts ts' |> List.for_all (fun (a, b) -> eq a b)
    | Poly t, Poly t' -> t = t'
    | Variant (targs, name), Variant (targs', name') ->
        List.for_all (fun (t, t') -> eq t t') (Util.zip targs targs')
        && name = name'
    | _ -> false

let unit_ty = Tuple []

let pervasive_vals =
    [ (["+"], Fun ([Int; Int], Int))
    ; (["-"], Fun ([Int; Int], Int))
    ; (["*"], Fun ([Int; Int], Int))
    ; (["/"], Fun ([Int; Int], Int))
    ; (["mod"], Fun ([Int; Int], Int))
    ; ([">"], Fun ([Int; Int], Bool))
    ; (["<"], Fun ([Int; Int], Bool))
    ; (["="], Fun ([Poly 0; Poly 0], Bool))
    ; ([";"], Fun ([Poly 0; Poly 1], Poly 1))
    ; (["."], Fun ([Variant ([Poly 0], ["array"]); Int], Poly 0))
    ; (["<neg>"], Fun ([Int], Int))
    ; ( ["<arrayassign>"]
      , Fun ([Variant ([Poly 0], ["array"]); Int; Poly 0], Tuple []) )
    ; (["not"], Fun ([Bool], Bool))
    ; (["ref"], Fun ([Poly 0], Variant ([Poly 0], ["ref"])))
    ; ([":="], Fun ([Variant ([Poly 0], ["ref"]); Poly 0], unit_ty)) ]

let pervasive_types =
    [ (["int"], Int)
    ; (["bool"], Bool)
    ; (["option"], Variant ([Poly 0], ["option"]))
    ; (["list"], Variant ([Poly 0], ["list"])) ]

type ctor_t = t list * t list * string list

let pervasive_ctors =
    [ (["Some"], ([Poly 0], [Poly 0], ["option"]))
    ; (["None"], ([], [Poly 0], ["option"]))
    ; (["::"], ([Poly 0; Variant ([Poly 0], ["list"])], [Poly 0], ["list"]))
    ; (["[]"], ([], [Poly 0], ["list"])) ]

let ids = List.mapi (fun i (id, _) -> (id, i))

let pervasive_val_ids = ids pervasive_vals

let pervasive_type_ids = ids pervasive_types

let pervasive_ctor_ids = List.mapi (fun i (id, _) -> (id, i)) pervasive_ctors
