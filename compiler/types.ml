type addresses_t = int list [@@deriving show]

type tid_t = Tid of int [@@deriving show]

type cid_t = Cid of int [@@deriving show]

type vid_t = Vid of int | VidSSA of int [@@deriving show]

type pid_t = Pid of int [@@deriving show]

type t =
    | Int
    | Str
    | Bool
    | Fun of t list * t
    | Tuple of t list
    | Var of var_t ref ref
    (* 多相型。intはタグ *)
    | Poly of pid_t
    | Variant of t list * tid_t

(* 不明の型。最初のintはlevelで2つめのintはデバッグ用のタグ *)
and var_t = Unknown of int * int * var_t ref list | Just of t * var_t ref list

let rec show = function
    | Int -> "Int"
    | Bool -> "Bool"
    | Str -> "Str"
    | Fun ([], ret) -> show ret
    | Fun ([arg], ret) -> "(" ^ show arg ^ " -> " ^ show ret ^ ")"
    | Fun (args, ret) ->
        "("
        ^ List.fold_left
            (fun ret t -> show t ^ " -> " ^ ret)
            (show ret) (List.rev args)
        ^ ")"
    | Tuple [] -> "()"
    | Tuple [t] -> "(" ^ show t ^ ")"
    | Tuple (t :: ts) ->
        "(" ^ List.fold_left (fun ts t -> ts ^ " * " ^ show t) (show t) ts ^ ")"
    | Poly (Pid i) -> "'" ^ string_of_int i
    | Var v -> show_var_t !(!v)
    | Variant ([], name) -> show_tid_t name
    | Variant ([t], name) -> show t ^ " " ^ show_tid_t name
    | Variant (t :: ts, name) ->
        "("
        ^ List.fold_left (fun ts t -> ts ^ ", " ^ show t) (show t) ts
        ^ ") " ^ show_tid_t name

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
    [ (["+"], Vid 0, Fun ([Int; Int], Int))
    ; (["-"], Vid 1, Fun ([Int; Int], Int))
    ; (["*"], Vid 2, Fun ([Int; Int], Int))
    ; (["/"], Vid 3, Fun ([Int; Int], Int))
    ; (["mod"], Vid 4, Fun ([Int; Int], Int))
    ; ([">"], Vid 5, Fun ([Int; Int], Bool))
    ; (["<"], Vid 6, Fun ([Int; Int], Bool))
    ; (["="], Vid 7, Fun ([Poly (Pid 0); Poly (Pid 0)], Bool))
    ; ([";"], Vid 8, Fun ([Poly (Pid 0); Poly (Pid 1)], Poly (Pid 1)))
    ; (["."], Vid 9, Fun ([Variant ([Poly (Pid 0)], Tid 2); Int], Poly (Pid 0)))
    ; (["<neg>"], Vid 10, Fun ([Int], Int))
    ; ( ["<arrayassign>"]
      , Vid 11
      , Fun ([Variant ([Poly (Pid 0)], Tid 2); Int; Poly (Pid 0)], Tuple []) )
    ; (["not"], Vid 12, Fun ([Bool], Bool))
    ; (["ref"], Vid 13, Fun ([Poly (Pid 0)], Variant ([Poly (Pid 0)], Tid 3)))
    ; ( [":="]
      , Vid 14
      , Fun ([Variant ([Poly (Pid 0)], Tid 3); Poly (Pid 0)], unit_ty) ) ]

let pervasive_types =
    [ (["int"], Tid 0, Int)
    ; (["bool"], Tid 1, Bool)
    ; (["array"], Tid 2, Variant ([Poly (Pid 0)], Tid 2))
    ; (["ref"], Tid 3, Variant ([Poly (Pid 0)], Tid 3))
    ; (["option"], Tid 4, Variant ([Poly (Pid 0)], Tid 4))
    ; (["list"], Tid 5, Variant ([Poly (Pid 0)], Tid 5)) ]

type ctor_t = t list * t list * string list

let pervasive_ctors =
    [ (["Some"], Cid 0, ([Poly (Pid 0)], [Poly (Pid 0)], Tid 4))
    ; (["None"], Cid 1, ([], [Poly (Pid 0)], Tid 4))
    ; ( ["::"]
      , Cid 2
      , ([Poly (Pid 0); Variant ([Poly (Pid 0)], Tid 5)], [Poly (Pid 0)], Tid 5)
      )
    ; (["[]"], Cid 3, ([], [Poly (Pid 0)], Tid 5)) ]
