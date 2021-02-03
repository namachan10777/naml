let parse f lexbuf =
    let open K6lex in
    let lexer () =
        let ante_position = lexbuf.pos in
        let token = lex lexbuf in
        let post_position = lexbuf.pos in
        (token, ante_position, post_position)
    in
    let parser = MenhirLib.Convert.Simplified.traditional2revised f in
    parser lexer

let parse_repl_string s =
    let buf =
        K6lex.create_lexbuf ~file:"no file" @@ Sedlexing.Utf8.from_string s
    in
    parse K6parser.repl buf

let parse_string s =
    let buf =
        K6lex.create_lexbuf ~file:"no file" @@ Sedlexing.Utf8.from_string s
    in
    parse K6parser.main buf

let eval_string s =
    let ast = parse_repl_string s in
    K6ast.eval (K6ast.init_ctx ()) ast
