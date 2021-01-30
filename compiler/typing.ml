(* Algorithm W*)

type id_t = Ast.id_t
[@@deriving show]

type pat_t = unit
[@@deriving show]

type t =
    | Int of int
    | Bool of bool
    | Var of id_t * Types.t ref 
    | App of t * t list * Types.t ref
[@@deriving show]

let rec lookup x = function
    | (y, ty) :: remain -> if x = y then ty else lookup x remain
    | _ -> raise @@ Failure "notfound"

let unify a b = match !a, !b with
    | Types.Int, Types.Int -> Types.Int
    | Types.Bool, Types.Bool -> Types.Bool
    | _ -> raise @@ Failure (Printf.sprintf "cannot unify %s %s" (Types.show !a) (Types.show !b))

let count = ref 0
let init () =
    count := 99

let fresh level =
    count := !count + 1;
    ref (Types.Unknown (level, !count))

let rec g env level =
    let (venv, tenv, cenv) = env in
    function
    | Ast.Int i -> Int i, ref Types.Int
    | Ast.Bool b -> Bool b, ref Types.Int
    | Ast.Var name -> Var (name, lookup name venv), lookup name venv
    | _ -> raise @@ Failure "unimlemented"

let f ast =
    fst @@ g (
        List.map (fun (n, ty) -> n, ref ty) Types.pervasive_vals,
        List.map (fun (n, ty) -> n, ref ty) Types.pervasive_types,
        List.map (fun (n, ty) -> n, ref ty) Types.pervasive_ctors
    ) 0 ast
