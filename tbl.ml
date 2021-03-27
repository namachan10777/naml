type ('a, 'b) t = ('a * 'b) list

let empty = []

let rec lookup key = function
    | (key', value) :: _ when key = key' -> Some value
    | _ :: tbl -> lookup key tbl
    | _ -> None

let expect msg = function Some v -> v | None -> failwith msg

let push key value tbl = (key, value) :: tbl

type ('a, 'b) mut_t = ('a * 'b) list ref

let rec lookup_mut key tbl = lookup key !tbl

let push_mut key value tbl =
    tbl := (key, value) :: !tbl

let make x = x
let make_mut x = ref x

let map f tbl = List.map (fun (key, v) -> (key, f v)) tbl

let concat l = List.concat l
