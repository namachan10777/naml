type t = string list * string * int

let show = function
    | [], name, id -> Printf.sprintf "%s(%d)" name id
    | prefix, name, id ->
        Printf.sprintf "%s(%d)"
          (List.fold_left
             (fun name ident -> ident ^ "." ^ name)
             name (List.rev prefix))
          id

let global_impure_count = ref 0

let from_strlist name =
    match List.rev name with
    | name :: prefix ->
        let id = (List.rev prefix, name, !global_impure_count) in
        global_impure_count := 1 + !global_impure_count ;
        id
    | [] -> failwith "invalid input to Id.t"

let approx (pre1, n1, _) (pre2, n2, _) = pre1 = pre2 && n1 = n2
