exception UnitTestError of string

let assert_eq name a b =
    print_endline @@ "testing \"" ^ name ^ "\"..." ;
    if a = b then () else raise (UnitTestError name)
