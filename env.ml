type 'a t = (Id.t * 'a) list
[@@deriving show]

exception NotFound

let get tbl (_, _, key) =
    let rec f = function
        | [] -> None
        | ((_, _, key'), value) :: _ when key = key' -> Some value
        | _ :: remain -> f remain
    in f tbl

let get_unwrap tbl key = match get tbl key with
    | Some v -> v
    | None -> raise Not_found

let push tbl key value =
    (key, value) :: tbl

let make tbl = tbl

let alpha_env l = Tbl.make @@ List.map (fun (id, _) -> (Id.name id, id)) l
let alpha_var_env l = Tbl.make @@ List.map (fun (id, _) -> (Id.name id, (id, true))) l
let map f tbl = List.map (fun (k, v) -> (k, f v)) tbl
let concat l = List.concat l
let names l = List.map fst l
let rec take_n n l = match n, l with
    | 0, l -> []
    | _, [] -> []
    | n, x :: xs -> x :: take_n (n-1) xs
let enclose_module before_ after prefix =
    let added = take_n (List.length after - List.length before_) after in
    List.map (fun ((pre, name, id), v) -> ((prefix @ pre, name, id), v)) added
