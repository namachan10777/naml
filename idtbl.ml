type ('a, 'b) t = ('a * 'b) list

let empty = []

let rec lookup ((_, _, id) as key) = function
    | ((_, _, id'), value) :: _ when id = id' -> Some value
    | _ :: tbl -> lookup key tbl
    | _ -> None

let expect msg = function Some v -> v | None -> failwith msg

let push key value tbl = (key, value) :: tbl

let map f = List.map (fun (k, v) -> (k, f v))

type ('a, 'b) mut_t = ('a * 'b) list ref

let rec lookup_mut key tbl = lookup key !tbl

let push_mut key value tbl =
    tbl := (key, value) :: !tbl
