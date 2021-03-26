type 'a t = (Id.t * 'a) list

let get tbl key =
    let rec f = function
        | [] -> None
        | (key', value) :: _ when key = key' -> Some value
        | _ :: remain -> f remain
    in f tbl

let get_unwrap tbl key = match get tbl key with
    | Some v -> v
    | None -> raise Not_found

let push tbl key value =
    (key, value) :: tbl
