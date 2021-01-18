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

let digit = [%sedlex.regexp? '0'..'9']
let space = [%sedlex.regexp? '\t' | ' ' | '\r' | '\n' ]
let integer = [%sedlex.regexp? Plus digit]
let identifier = [%sedlex.regexp? (alphabetic | "_"), Star (alphabetic | "_" | "." | "'" | '0'..'9')]

open K5parser

let rec lex lexbuf =
    let buf = lexbuf.stream in
    match%sedlex buf with
    | space ->
        update lexbuf;
        new_line lexbuf;
        lex lexbuf
    (* symbols *)
    | "|" ->
        update lexbuf;
        VBar
    | "(" ->
        update lexbuf;
        LParen
    | ")" ->
        update lexbuf;
        RParen
    | "[" ->
        update lexbuf;
        LBra
    | "]" ->
        update lexbuf;
        RBra
    | "+" ->
        update lexbuf;
        Plus
    | "-" ->
        update lexbuf;
        Minus
    | "*" ->
        update lexbuf;
        Asterisk
    | "/" ->
        update lexbuf;
        Slash
    | "=" ->
        update lexbuf;
        Equal
    | "!=" ->
        update lexbuf;
        Neq
    | "||" ->
        update lexbuf;
        Or
    | "&&" ->
        update lexbuf;
        And
    | '!' ->
        update lexbuf;
        Not
    | "<" ->
        update lexbuf;
        Less
    | ">" ->
        update lexbuf;
        Gret
    | ";" ->
        update lexbuf;
        Semicolon
    | "::" ->
        update lexbuf;
        ColCol
    | "->" ->
        update lexbuf;
        Arrow
    | "<>" ->
        update lexbuf;
        NotEq
    (* keywords *)
    | "fun" ->
        update lexbuf;
        Fun
    | "let" ->
        update lexbuf;
        Let
    | "rec" ->
        update lexbuf;
        Rec
    | "in" ->
        update lexbuf;
        In
    | "if" ->
        update lexbuf;
        If
    | "then" ->
        update lexbuf;
        Then
    | "else" ->
        update lexbuf;
        Else
    | "match" ->
        update lexbuf;
        Match
    | "with" ->
        update lexbuf;
        With
    | "List.hd" ->
        update lexbuf;
        Head
    | "List.tl" ->
        update lexbuf;
        Tail
    (* literals *)
    | "true" ->
        update lexbuf;
        True
    | "false" ->
        update lexbuf;
        False
    | "debug_print" ->
        update lexbuf;
        DebugPrint
    | integer ->
        update lexbuf;
        Int (int_of_string @@ lexeme lexbuf)
    | identifier ->
        update lexbuf;
        Var (lexeme lexbuf)
    | eof ->
        update lexbuf;
        Eof
    | _ -> raise_parse_error lexbuf
