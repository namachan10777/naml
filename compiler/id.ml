type t = string list * int

let rec show (path, id) =
    let rec f = function
        | [] -> ""
        | [id] -> id
        | h :: remain -> h ^ "." ^ f remain
    in
    f path ^ "(" ^ string_of_int id ^ ")"
