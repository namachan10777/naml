let parse f lexbuf =
    let open K7lex in
    let lexer () =
        let ante_position = lexbuf.pos in
        let token = lex lexbuf in
        let post_position = lexbuf.pos
        in (token, ante_position, post_position) in
    let parser =
        MenhirLib.Convert.Simplified.traditional2revised f
    in
    parser lexer

let parse_repl_string s =
    let buf = K7lex.create_lexbuf ~file:"no file"
        @@ Sedlexing.Utf8.from_string s in
    parse K7parser.repl buf

let parse_string s =
    let buf = K7lex.create_lexbuf ~file:"no file"
        @@ Sedlexing.Utf8.from_string s in
    parse K7parser.main buf

let eval_string s =
    let ast = parse_repl_string s in
    K7ast.eval (K7ast.init_ctx ()) ast

let typecheck_string s =
    let ast = parse_repl_string s in
    K7ast.tcheck ast
