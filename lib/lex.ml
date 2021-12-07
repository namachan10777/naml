type pos_t = string * int * int * int

let show_pos_t (fname, l, c, _) = Printf.sprintf "%s:%d:%d" fname l c

let pp_pos_t f p = Format.pp_print_string f (show_pos_t p)

let string_of_pos_t (file, line, col, _) =
  Printf.sprintf "%s:%d:%d" file col line

let nowhere = ("", -1, -1, -1)

let initial_pos fname = (fname, 1, 0, 0)

type t =
  | Str of string
  | Int of int
  | UIdent of string
  | Char of char
  | LIdent of string
  | TVar of string
  | TInt
  | TBool
  | TString
  | Type
  | Of
  | Dot
  | True
  | False
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Cons
  | Gret
  | Less
  | Eq
  | Deref
  | Neq
  | And
  | Or
  | Pipeline
  | AtAt
  | Assign
  | ArrayAssign
  | LP
  | RP
  | LB
  | RB
  | Semicol
  | Comma
  | VBar
  | Arrow
  | Fun
  | If
  | Then
  | Else
  | Let
  | In
  | Rec
  | AndDef
  | Match
  | When
  | With
  | Not
  | Ref
  | As
  | Eof
[@@deriving show]

let show = function
  | Str s ->
      Printf.sprintf "Str(%s)" s
  | Int n ->
      Printf.sprintf "Int(%d)" n
  | Char c ->
      Printf.sprintf "Char(%c)" c
  | True ->
      "true"
  | False ->
      "false"
  | TInt ->
      "int"
  | TBool ->
      "bool"
  | TString ->
      "string"
  | Type ->
      "type"
  | Of ->
      "of"
  | Dot ->
      "."
  | Add ->
      "+"
  | Sub ->
      "-"
  | Mul ->
      "*"
  | Div ->
      "/"
  | Mod ->
      "mod"
  | Cons ->
      "::"
  | Gret ->
      ">"
  | Less ->
      "<"
  | Eq ->
      "="
  | Deref ->
      "!"
  | Neq ->
      "<>"
  | And ->
      "&&"
  | Or ->
      "||"
  | Pipeline ->
      "|>"
  | AtAt ->
      "@@"
  | Assign ->
      ":="
  | ArrayAssign ->
      ":=(array)"
  | LP ->
      "("
  | RP ->
      ")"
  | LB ->
      "["
  | RB ->
      "]"
  | Semicol ->
      ";"
  | Comma ->
      ","
  | VBar ->
      "|"
  | Arrow ->
      "->"
  | Fun ->
      "fun"
  | If ->
      "if"
  | Then ->
      "then"
  | Else ->
      "else"
  | Let ->
      "let"
  | In ->
      "in"
  | Rec ->
      "rec"
  | AndDef ->
      "and"
  | Match ->
      "match"
  | When ->
      "when"
  | With ->
      "with"
  | Not ->
      "not"
  | Ref ->
      "ref"
  | As ->
      "as"
  | Eof ->
      "<EOF>"
  | UIdent id ->
      id
  | LIdent id ->
      id
  | TVar id ->
      Printf.sprintf "'%s" id

let count_newline s =
  let rec cnt acc i_begin i_end =
    if i_begin == i_end then acc
    else if s.[i_begin] = '\n' then cnt (acc + 1) (i_begin + 1) i_end
    else cnt acc (i_begin + 1) i_end
  in
  cnt 0

let match_space_char s i =
  if i >= String.length s then None
  else if s.[i] = ' ' || s.[i] = '\t' || s.[i] = '\n' || s.[i] = '\r' then
    Some (i + 1)
  else None

let match_alph_char s i =
  if i >= String.length s then None
  else
    let code = Char.code s.[i] in
    if (code >= 0x41 && code <= 0x5a) || (code >= 0x61 && code <= 0x7a) then
      Some (i + 1)
    else None

let match_alph_lower_char s i =
  if i >= String.length s then None
  else
    let code = Char.code s.[i] in
    if code >= 0x61 && code <= 0x7a then Some (i + 1) else None

let match_alph_upper_char s i =
  if i >= String.length s then None
  else
    let code = Char.code s.[i] in
    if code >= 0x41 && code <= 0x5a then Some (i + 1) else None

let match_num_char s i =
  if i >= String.length s then None
  else
    let code = Char.code s.[i] in
    if code >= 0x30 && code <= 0x39 then Some (i + 1) else None

let match_hexnum_char s i =
  if i >= String.length s then None
  else
    let code = Char.code s.[i] in
    if
      (code >= 0x30 && code <= 0x39)
      || (code >= 0x41 && code <= 0x46)
      || (code >= 0x61 && code <= 0x66)
    then Some (i + 1)
    else None

let match_char c s i =
  if i >= String.length s then None
  else if c = s.[i] then Some (i + 1)
  else None

let match_str pat s i =
  if i + String.length pat > String.length s then None
  else if pat = String.sub s i (String.length pat) then
    Some (i + String.length pat)
  else None

let opt pat s =
  let rec f acc i =
    match (acc, pat s i) with
    | _, Some res ->
        f (Some res) (i + 1)
    | before, None ->
        before
  in
  f None

let star pat s i =
  let rec f acc i =
    match (acc, pat s i) with
    | _, Some res ->
        f (Some res) (i + 1)
    | before, None ->
        before
  in
  f (Some i) i

let comb_or pat1 pat2 s i =
  match pat1 s i with Some i -> Some i | None -> pat2 s i

let match_strlit s i =
  let rec f i =
    if i >= String.length s then None
    else if i + 1 < String.length s && s.[i] = '\\' && s.[i + 1] = '\"' then
      f (i + 2)
    else if s.[i] = '"' then Some (i + 1)
    else f (i + 1)
  in
  if i + 1 >= String.length s then None
  else if s.[i] = '"' then f (i + 1)
  else None

let match_charlit s i =
  if
    i + 3 < String.length s
    && s.[i] = '\''
    && s.[i + 1] == '\\'
    && s.[i + 3] == '\''
  then Some (i + 4)
  else if i + 2 < String.length s && s.[i] = '\'' && s.[i + 2] == '\'' then
    Some (i + 3)
  else None

let chain pat1 pat2 s i =
  match pat1 s i with Some i -> pat2 s i | None -> None

let match_space = opt match_space_char

let match_lower_ident =
  chain match_alph_lower_char
  @@ star (comb_or match_alph_char (comb_or match_num_char (match_char '_')))

let match_upper_ident =
  chain match_alph_upper_char
  @@ star (comb_or match_alph_char (comb_or match_num_char (match_char '_')))

let match_tvar = chain (match_char '\'') match_lower_ident

let match_int = opt match_num_char

let match_hexint = chain (match_str "0x") (opt match_hexnum_char)

let update_pos s (fname, line, col, i_start) i_end =
  let rec f (col, el_cnt) i =
    if i >= i_end then (col, el_cnt)
    else if s.[i] = '\n' then f (0, el_cnt + 1) (i + 1)
    else f (col + 1, el_cnt) (i + 1)
  in
  let col, el_cnt = f (col, 0) i_start in
  (fname, line + el_cnt, col, i_end)

exception LexException of pos_t

let rec lex s pos =
    let (_, _, _, i) = pos in
    let take e = String.sub s i (e-i) in
    if i = String.length s
    then [Eof]
    else
        let pos = update_pos s pos in
        match match_strlit s i with
        | Some i' ->
            let inner = take i' in
            Str (String.sub inner 1 ((String.length inner) - 2)) :: lex s (pos i')
        | None -> match match_charlit s i with
        | Some i' ->
            let inner = take i' in
            Char (inner.[(String.length inner) - 2]) :: lex s (pos i')
        | None -> match match_tvar s i with
        | Some i' -> let inner = take i' in
            TVar (String.sub inner 1 ((String.length inner) - 1)) :: lex s (pos i')
        | None -> match match_space s i with
        | Some _ -> lex s (pos (i+1))
        | None -> match match_hexint s i with
        | Some i' ->
            Int (int_of_string @@ take i') :: lex s (pos i')
        | None -> match match_int s i with
        | Some i' ->
            Int (int_of_string @@ take i') :: lex s (pos i')

        (* 愚直すぎ (末尾再帰の最適化を狙っています。許して) *)
        | None -> match match_str "." s i with
        | Some i' -> Dot :: lex s (pos i')
        | None -> match match_str "::" s i with
        | Some i' -> Cons :: lex s (pos i')
        | None -> match match_str "->" s i with
        | Some i' -> Arrow :: lex s (pos i')
        | None -> match match_str "<-" s i with
        | Some i' -> ArrayAssign :: lex s (pos i')
        | None -> match match_str ":=" s i with
        | Some i' -> Assign :: lex s (pos i')
        | None -> match match_str "+" s i with
        | Some i' -> Add :: lex s (pos i')
        | None -> match match_str "-" s i with
        | Some i' -> Sub :: lex s (pos i')
        | None -> match match_str "*" s i with
        | Some i' -> Mul :: lex s (pos i')
        | None -> match match_str "/" s i with
        | Some i' -> Div :: lex s (pos i')
        | None -> match match_str "|>" s i with
        | Some i' -> Pipeline :: lex s (pos i')
        | None -> match match_str "@@" s i with
        | Some i' -> AtAt :: lex s (pos i')
        | None -> match match_str "||" s i with
        | Some i' -> Or :: lex s (pos i')
        | None -> match match_str "&&" s i with
        | Some i' -> And :: lex s (pos i')
        | None -> match match_str "=" s i with
        | Some i' -> Eq :: lex s (pos i')
        | None -> match match_str "!" s i with
        | Some i' -> Deref :: lex s (pos i')
        | None -> match match_str "<>" s i with
        | Some i' -> Neq :: lex s (pos i')
        | None -> match match_str ">" s i with
        | Some i' -> Gret :: lex s (pos i')
        | None -> match match_str "<" s i with
        | Some i' -> Less :: lex s (pos i')
        | None -> match match_str "(" s i with
        | Some i' -> LP :: lex s (pos i')
        | None -> match match_str ")" s i with
        | Some i' -> RP :: lex s (pos i')
        | None -> match match_str "[" s i with
        | Some i' -> LB :: lex s (pos i')
        | None -> match match_str "]" s i with
        | Some i' -> RB :: lex s (pos i')
        | None -> match match_str ";" s i with
        | Some i' -> Semicol :: lex s (pos i')
        | None -> match match_str "," s i with
        | Some i' -> Comma :: lex s (pos i')
        | None -> match match_str "|" s i with
        | Some i' -> VBar :: lex s (pos i')
        | None -> match match_upper_ident s i with
        | Some i' -> UIdent (take i') :: lex s (pos i')
        | None -> match match_lower_ident s i with
        | Some i' -> begin match take i' with
            | "and" -> AndDef :: lex s (pos i')
            | "type" -> Type :: lex s (pos i')
            | "of" -> Of :: lex s (pos i')
            | "as" -> As :: lex s (pos i')
            | "int" -> TInt :: lex s (pos i')
            | "bool" -> TBool :: lex s (pos i')
            | "string" -> TString :: lex s (pos i')
            | "true" -> True :: lex s (pos i')
            | "false" -> False :: lex s (pos i')
            | "if" -> If :: lex s (pos i')
            | "then" -> Then :: lex s (pos i')
            | "else" -> Else :: lex s (pos i')
            | "let" -> Let :: lex s (pos i')
            | "rec" -> Rec :: lex s (pos i')
            | "in" -> In :: lex s (pos i')
            | "fun" -> Fun :: lex s (pos i')
            | "match" -> Match :: lex s (pos i')
            | "when" -> When :: lex s (pos i')
            | "with" -> With :: lex s (pos i')
            | "mod" -> Mod :: lex s (pos i')
            | ident -> LIdent ident :: lex s (pos i')
        end
        | None -> Printf.printf "lex: %s %d\n" s i; raise (LexException (pos 0))
[@@ocamlformat "disable"]

let f fname src = lex src @@ initial_pos fname
