type pos_t = string * int * int * int [@@deriving show]

let initial_pos fname = (fname, 1, 1, 0)

type t =
    | Str of string * pos_t
    | Int of int * pos_t
    | UIdent of string * pos_t
    | Char of char * pos_t
    | LIdent of string * pos_t
    | TVar of string * pos_t
    | TInt of pos_t
    | TBool of pos_t
    | TString of pos_t
    | Type of pos_t
    | Of of pos_t
    | Dot of pos_t
    | True of pos_t
    | False of pos_t
    | Add of pos_t
    | Sub of pos_t
    | Mul of pos_t
    | Div of pos_t
    | Mod of pos_t
    | Cons of pos_t
    | Gret of pos_t
    | Less of pos_t
    | Eq of pos_t
    | Neq of pos_t
    | And of pos_t
    | Or of pos_t
    | Pipeline of pos_t
    | AtAt of pos_t
    | Assign of pos_t
    | ArrayAssign of pos_t
    | LP of pos_t
    | RP of pos_t
    | LB of pos_t
    | RB of pos_t
    | Semicol of pos_t
    | Comma of pos_t
    | VBar of pos_t
    | Arrow of pos_t
    | Fun of pos_t
    | If of pos_t
    | Then of pos_t
    | Else of pos_t
    | Let of pos_t
    | In of pos_t
    | Rec of pos_t
    | AndDef of pos_t
    | Match of pos_t
    | When of pos_t
    | With of pos_t
    | Builtin of pos_t
    | Not of pos_t
    | Ref of pos_t
    | As of pos_t
    | Eof of pos_t
[@@deriving show]

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
        | _, Some res -> f (Some res) (i + 1)
        | before, None -> before
    in
    f None

let star pat s i =
    let rec f acc i =
        match (acc, pat s i) with
        | _, Some res -> f (Some res) (i + 1)
        | before, None -> before
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

let update_pos s (fname, line, col, _) i = (fname, line, col, i)

exception LexException of pos_t

let rec lex s pos =
    let (_, _, _, i) = pos in
    let take e = String.sub s i (e-i) in
    if i = String.length s
    then [Eof pos]
    else
        match match_strlit s i with
        | Some i ->
            let inner = take i in
            Str (String.sub inner 1 ((String.length inner) - 2), pos) :: lex s (update_pos s pos i)
        | None -> match match_charlit s i with
        | Some i ->
            let inner = take i in
            Char (inner.[(String.length inner) - 2], pos) :: lex s (update_pos s pos i)
        | None -> match match_tvar s i with
        | Some i -> let inner = take i in
            TVar (String.sub inner 1 ((String.length inner) - 1), pos) :: lex s (update_pos s pos i)
        | None -> match match_space s i with
        | Some i -> lex s (update_pos s pos i)
        | None -> match match_hexint s i with
        | Some i ->
            Int (int_of_string @@ take i, pos) :: lex s (update_pos s pos i)
        | None -> match match_int s i with
        | Some i ->
            Int (int_of_string @@ take i, pos) :: lex s (update_pos s pos i)

        (* 愚直すぎ (末尾再帰の最適化を狙っています。許して) *)
        | None -> match match_str "." s i with
        | Some i -> Dot pos :: lex s (update_pos s pos i)
        | None -> match match_str "pos ::" s i with
        | Some i -> Cons pos :: lex s (update_pos s pos i)
        | None -> match match_str "->" s i with
        | Some i -> Arrow pos :: lex s (update_pos s pos i)
        | None -> match match_str "<-" s i with
        | Some i -> ArrayAssign pos :: lex s (update_pos s pos i)
        | None -> match match_str ":=" s i with
        | Some i -> Assign pos :: lex s (update_pos s pos i)
        | None -> match match_str "+" s i with
        | Some i -> Add pos :: lex s (update_pos s pos i)
        | None -> match match_str "-" s i with
        | Some i -> Sub pos :: lex s (update_pos s pos i)
        | None -> match match_str "*" s i with
        | Some i -> Mul pos :: lex s (update_pos s pos i)
        | None -> match match_str "/" s i with
        | Some i -> Div pos :: lex s (update_pos s pos i)
        | None -> match match_str "|>" s i with
        | Some i -> Pipeline pos :: lex s (update_pos s pos i)
        | None -> match match_str "@@" s i with
        | Some i -> AtAt pos :: lex s (update_pos s pos i)
        | None -> match match_str "||" s i with
        | Some i -> Or pos :: lex s (update_pos s pos i)
        | None -> match match_str "&&" s i with
        | Some i -> And pos :: lex s (update_pos s pos i)
        | None -> match match_str "=" s i with
        | Some i -> Eq pos :: lex s (update_pos s pos i)
        | None -> match match_str "<>" s i with
        | Some i -> Neq pos :: lex s (update_pos s pos i)
        | None -> match match_str ">" s i with
        | Some i -> Gret pos :: lex s (update_pos s pos i)
        | None -> match match_str "<" s i with
        | Some i -> Less pos :: lex s (update_pos s pos i)
        | None -> match match_str "(" s i with
        | Some i -> LP pos :: lex s (update_pos s pos i)
        | None -> match match_str ")" s i with
        | Some i -> RP pos :: lex s (update_pos s pos i)
        | None -> match match_str "[" s i with
        | Some i -> LB pos :: lex s (update_pos s pos i)
        | None -> match match_str "]" s i with
        | Some i -> RB pos :: lex s (update_pos s pos i)
        | None -> match match_str ";" s i with
        | Some i -> Semicol pos :: lex s (update_pos s pos i)
        | None -> match match_str "," s i with
        | Some i -> Comma pos :: lex s (update_pos s pos i)
        | None -> match match_str "|" s i with
        | Some i -> VBar pos :: lex s (update_pos s pos i)
        | None -> match match_upper_ident s i with
        | Some i -> UIdent (take i, pos) :: lex s (update_pos s pos i)
        | None -> match match_lower_ident s i with
        | Some i -> begin match take i with
            | "and" -> AndDef pos :: lex s (update_pos s pos i)
            | "type" -> Type pos :: lex s (update_pos s pos i)
            | "of" -> Of pos :: lex s (update_pos s pos i)
            | "as" -> As pos :: lex s (update_pos s pos i)
            | "int" -> TInt pos :: lex s (update_pos s pos i)
            | "bool" -> TBool pos :: lex s (update_pos s pos i)
            | "string" -> TString pos :: lex s (update_pos s pos i)
            | "true" -> True pos :: lex s (update_pos s pos i)
            | "false" -> False pos :: lex s (update_pos s pos i)
            | "if" -> If pos :: lex s (update_pos s pos i)
            | "then" -> Then pos :: lex s (update_pos s pos i)
            | "else" -> Else pos :: lex s (update_pos s pos i)
            | "let" -> Let pos :: lex s (update_pos s pos i)
            | "rec" -> Rec pos :: lex s (update_pos s pos i)
            | "in" -> In pos :: lex s (update_pos s pos i)
            | "fun" -> Fun pos :: lex s (update_pos s pos i)
            | "match" -> Match pos :: lex s (update_pos s pos i)
            | "when" -> When pos :: lex s (update_pos s pos i)
            | "with" -> With pos :: lex s (update_pos s pos i)
            | "builtin" -> Builtin pos :: lex s (update_pos s pos i)
            | "mod" -> Mod pos :: lex s (update_pos s pos i)
            | ident -> LIdent (ident, pos) :: lex s (update_pos s pos i)
        end
        | None -> raise (LexException pos)
[@@ocamlformat "disable"]

let f fname src = lex src @@ initial_pos fname
