let parse f lexbuf =
    let open Kadai4_lex in
    let lexer () =
        let ante_position = lexbuf.pos in
        let token = lex lexbuf in
        let post_position = lexbuf.pos
        in (token, ante_position, post_position) in
    let parser =
        MenhirLib.Convert.Simplified.traditional2revised f
    in
    parser lexer

let parse_string s =
    let buf = Kadai4_lex.create_lexbuf ~file:"no file"
        @@ Sedlexing.Utf8.from_string s in
    parse Kadai4_parser.main buf
