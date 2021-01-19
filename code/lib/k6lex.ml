type lexbuf_t = {
    stream: Sedlexing.lexbuf;
    mutable pos: Lexing.position;
}

let create_lexbuf ?(file="") stream =
    let pos = {
        Lexing.pos_fname = file;
        Lexing.pos_lnum = 1;
        Lexing.pos_bol = 0;
        Lexing.pos_cnum = 0;
    }
    in { pos; stream }

let new_line lexbuf =
    let open Lexing in
    let lcp = lexbuf.pos in
    lexbuf.pos <- {
        lcp with
        pos_lnum = lcp.pos_lnum + 1;
        pos_bol  = lcp.pos_cnum;
    }

let update lexbuf =
    let new_pos = Sedlexing.lexeme_end lexbuf.stream in
    let p = lexbuf.pos in
    lexbuf.pos <- { p with Lexing.pos_cnum = new_pos }

let lexeme {stream;_} = Sedlexing.Utf8.lexeme stream

exception ParseError of (string * int * int * string)
let raise_parse_error lexbuf =
    let {pos;_} = lexbuf in
    let line = pos.pos_lnum in
    let col = pos.pos_cnum - pos.pos_bol in
    let tok = lexeme lexbuf in
    raise @@ ParseError (pos.pos_fname, line, col, tok)

let identifier = [%sedlex.regexp? (alphabetic | "_"), Star (alphabetic | "_" | "." | "'" | '0'..'9')]
let integer = [%sedlex.regexp? Plus ('0'..'9')]
let space = [%sedlex.regexp? '\t' | ' ' | '\r' | '\n' ]
let str = [%sedlex.regexp? '"', Star ("\\\"" | any), '"']

open K6parser

let rec lex lexbuf =
    let buf = lexbuf.stream in
    match%sedlex buf with
    | space ->
        update lexbuf;
        new_line lexbuf;
        lex lexbuf
    | "->" ->
        update lexbuf;
        new_line lexbuf;
        Arrow
    | "+" ->
        update lexbuf;
        new_line lexbuf;
        Add
    | "-" ->
        update lexbuf;
        new_line lexbuf;
        Sub
    | "*" ->
        update lexbuf;
        new_line lexbuf;
        Mul
    | "/" ->
        update lexbuf;
        new_line lexbuf;
        Div
    | ";" ->
        update lexbuf;
        new_line lexbuf;
        Semicol
    | "," ->
        update lexbuf;
        new_line lexbuf;
        Comma
    | "|" ->
        update lexbuf;
        new_line lexbuf;
        VBar
    | '(' ->
        update lexbuf;
        new_line lexbuf;
        LP
    | ')' ->
        update lexbuf;
        new_line lexbuf;
        RP
    | '[' ->
        update lexbuf;
        new_line lexbuf;
        LB
    | ']' ->
        update lexbuf;
        new_line lexbuf;
        RB
    | "let" ->
        update lexbuf;
        new_line lexbuf;
        Let
    | "=" ->
        update lexbuf;
        new_line lexbuf;
        Eq
    | "in" ->
        update lexbuf;
        new_line lexbuf;
        In
    | "match" ->
        update lexbuf;
        new_line lexbuf;
        Match
    | "with" ->
        update lexbuf;
        new_line lexbuf;
        With
    | "builtin" ->
        update lexbuf;
        new_line lexbuf;
        Builtin
    | str -> 
        update lexbuf;
        new_line lexbuf;
        let s = lexeme lexbuf in
        Str (String.sub s 1 ((String.length s) - 2))
    | integer ->
        update lexbuf;
        new_line lexbuf;
        Int (int_of_string @@ lexeme lexbuf)
    | identifier ->
        update lexbuf;
        Ident (lexeme lexbuf)
    | eof ->
        update lexbuf;
        Eof
    | _ -> raise_parse_error lexbuf
