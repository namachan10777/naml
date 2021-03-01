type ('a, 'b) t = ('a * 'b) list

let empty = []

let rec lookup key = function
    | (key', value) :: _ when key = key' -> Some value
    | _ :: tbl -> lookup key tbl
    | _ -> None

let expect msg = function Some v -> v | None -> failwith msg

let push key value tbl = (key, value) :: tbl

let map f = List.map (fun (k, v) -> (k, f v))

type ('a, 'b) mut_t = ('a * 'b) list ref

let rec lookup_mut key tbl = lookup key !tbl

let push_mut key value tbl =
    tbl := (key, value) :: !tbl

let lookup_mut_or key tbl value =
    let rec f = function
        | (key', value) :: _ when key = key' -> value
        | _ :: tbl -> f tbl
        | [] -> push_mut key value tbl; value
    in f !tbl
