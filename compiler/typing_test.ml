let test src expected =
    let s, _ = Parser.parse_expr @@ Lex.lex src @@ Lex.initial_pos "test.ml" in
    let ast = Ast.of_parser_t s in
    let typed = Typing.f ast in
    if typed = expected
    then ()
    else begin
        print_endline "-------------------------------------";
        Printf.printf "left: \n%s\n" @@ Typing.show typed;
        Printf.printf "right: \n%s\n" @@ Typing.show expected;
    end

module Ty = Types

let () =
    test "0" (Typing.Int 0);
    test "not" (Typing.Var (["not"], ref (Ty.Fun ([ref Ty.Bool], ref Ty.Bool))));
