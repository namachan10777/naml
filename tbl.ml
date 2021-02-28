type ('a, 'b) t = ('a * 'b) list

let empty = []

let rec lookup key = function
    | (key', value) :: _ when key = key' -> Some value
    | _ :: tbl -> lookup key tbl
    | _ -> None

let expect msg = function Some v -> v | None -> failwith msg

let push key value tbl = (key, value) :: tbl

let map f = List.map (fun (k, v) -> (k, f v))
