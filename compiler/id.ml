type t = string list

let rec show = function
    | [] -> ""
    | [id] -> id
    | h :: remain -> h ^ "." ^ show remain
