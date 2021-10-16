type t =
    | Never
    | Int
    | Bool
    | Str
    | Fun of t * t
    | Tuple of t list
    | Poly of int
    | Variant of t list * Id.t

let rec show = function
    | Never -> "never"
    | Int -> "int"
    | Bool -> "bool"
    | Str -> "string"
    | Fun (Fun _ as arg, ret) -> Printf.sprintf "(%s) -> %s" (show arg) (show ret)
    | Fun (arg, ret) -> Printf.sprintf "%s -> %s" (show arg) (show ret)
    | Tuple ts -> begin match List.map (
            function
            | (Fun _ | Tuple _) as t -> Printf.sprintf "(%s)" @@ show t
            | t -> show t
        ) ts with
        | [] -> "unit"
        | head :: tail -> List.fold_left (fun acc t -> acc ^ " * " ^ t) head tail
    end
    | Poly id -> Printf.sprintf "'%d" id
    | Variant ([], id) -> Id.show id
    | Variant ([arg], id) -> Printf.sprintf "%s %s" (show arg) (Id.show id)
    | Variant (arg_head :: arg_tail, id) -> Printf.sprintf "(%s) %s" (List.fold_left (fun acc t -> acc ^ ", " ^ show t) (show arg_head) arg_tail) (Id.show id)

let pp fmt t = Format.fprintf fmt "%s" (show t)

let unit = Tuple []

type scheme_t = int * t
