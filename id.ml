type t = string list * string * int [@@deriving show]

let show = function
    | [], name, id -> Printf.sprintf "%s(%d)" name id
    | prefix, name, id ->
        Printf.sprintf "%s(%d)"
          (List.fold_left
             (fun name ident -> ident ^ "." ^ name)
             name (List.rev prefix))
          id

let global_impure_count = ref 0

let name (pre, name, _) = pre @ [name]

let from_strlist name =
    match List.rev name with
    | name :: prefix ->
        let id = (List.rev prefix, name, !global_impure_count) in
        global_impure_count := 1 + !global_impure_count ;
        id
    | [] -> failwith "invalid input to Id.t"

let approx (pre1, n1, _) (pre2, n2, _) = pre1 = pre2 && n1 = n2

let rec lookup l = function
    | (pre, name, id) :: _ when l = pre @ [name] -> (pre, name, id)
    | _ :: tbl -> lookup l tbl
    | [] -> (
      match List.rev l with
      | name :: pre ->
          failwith
          @@ Printf.sprintf "undefined variable: %s"
               (List.fold_right
                  (fun acc p -> p ^ "." ^ acc)
                  (List.rev pre) name)
      | [] -> failwith "empty name" )
