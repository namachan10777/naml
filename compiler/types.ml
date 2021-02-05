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
    | Variant of t list * int

and var_t = Unknown of int * int * var_t ref list | Just of t * var_t ref list

let rec show = function
    | Int -> "Int"
    | Bool -> "Bool"
    | Str -> "Str"
    | Fun ([], ret) -> show ret
    | Fun ([arg], ret) -> show arg ^ " -> " ^ show ret
    | Fun (args, ret) ->
        List.fold_left
          (fun ret t -> show t ^ " -> " ^ ret)
          (show ret) (List.rev args)
    | Tuple [] -> "()"
    | Tuple [t] -> "(" ^ show t ^ ")"
    | Tuple (t :: ts) ->
        "(" ^ List.fold_left (fun ts t -> ts ^ " * " ^ show t) (show t) ts ^ ")"
    | Poly i -> "'" ^ string_of_int i
    | Var v -> show_var_t !(!v)
    | Variant ([], name) -> string_of_int name
    | Variant ([t], name) -> show t ^ " " ^ string_of_int name
    | Variant (t :: ts, name) ->
        "("
        ^ List.fold_left (fun ts t -> ts ^ ", " ^ show t) (show t) ts
        ^ ") " ^ string_of_int name

and show_var_t = function
    | Unknown (level, tag, refs) -> Printf.sprintf "Unknown (%d, %d)" level tag
    | Just (t, refs) -> "Just (" ^ show t ^ ")"

let pp fmt t = Ppx_deriving_runtime.Format.fprintf fmt "%s" @@ show t

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
    [ (["+"], 0, Fun ([Int; Int], Int))
    ; (["-"], 1, Fun ([Int; Int], Int))
    ; (["*"], 2, Fun ([Int; Int], Int))
    ; (["/"], 3, Fun ([Int; Int], Int))
    ; (["mod"], 4, Fun ([Int; Int], Int))
    ; ([">"], 5, Fun ([Int; Int], Bool))
    ; (["<"], 6, Fun ([Int; Int], Bool))
    ; (["="], 7, Fun ([Poly 0; Poly 0], Bool))
    ; ([";"], 8, Fun ([Poly 0; Poly 1], Poly 1))
    ; (["."], 9, Fun ([Variant ([Poly 0], 2); Int], Poly 0))
    ; (["<neg>"], 10, Fun ([Int], Int))
    ; ( ["<arrayassign>"]
      , 11, Fun ([Variant ([Poly 0], 2); Int; Poly 0], Tuple []) )
    ; (["not"], 12, Fun ([Bool], Bool))
    ; (["ref"], 13, Fun ([Poly 0], Variant ([Poly 0], 3)))
    ; ([":="], 14, Fun ([Variant ([Poly 0], 3); Poly 0], unit_ty)) ]

let pervasive_types =
    [ (["int"], 0, Int)
    ; (["bool"], 1, Bool)
    ; (["array"], 2, Variant ([Poly 0], 2))
    ; (["ref"], 3, Variant ([Poly 0], 3))
    ; (["option"], 4, Variant ([Poly 0], 4))
    ; (["list"], 5, Variant ([Poly 0], 5)) ]

type ctor_t = t list * t list * string list

let pervasive_ctors =
    [ (["Some"], 0, ([Poly 0], [Poly 0], 4))
    ; (["None"], 1, ([], [Poly 0], 4))
    ; (["::"], 2, ([Poly 0; Variant ([Poly 0], 5)], [Poly 0], 5))
    ; (["[]"], 3, ([], [Poly 0], 5)) ]
