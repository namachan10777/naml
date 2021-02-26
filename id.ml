type t = string list * string * int

let show = function
    | [], name, id -> Printf.sprintf "%s(%d)" name id
    | prefix, name, id ->
        Printf.sprintf "%s(%d)"
          (List.fold_left
             (fun name ident -> ident ^ "." ^ name)
             name (List.rev prefix))
          id
