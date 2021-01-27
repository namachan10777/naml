type pos_t = string * int * int * int
let initial_pos fname = (fname, 1, 1, 0)

let count_newline s =
    let rec cnt acc i_begin i_end =
        if i_begin == i_end
        then acc
        else if s.[i_begin] = '\n'
        then cnt (acc+1) (i_begin+1) i_end
        else cnt acc (i_begin+1) i_end
    in cnt 0

let match_space_char s i =
    if i >= String.length s
    then None
    else if s.[i] = ' ' || s.[i] = '\t' || s.[i] = '\n' || s.[i] = '\r'
    then Some (i+1)
    else None

let match_alph_char s i =
    if i >= String.length s
    then None
    else
        let code = Char.code s.[i] in
        if code >= 0x41 && code <= 0x5a || code >= 61 && code <= 0x7a
        then Some(i+1)
        else None

let match_num_char s i =
    if i >= String.length s
    then None
    else
        let code = Char.code s.[i] in
        if code >= 0x30 && code <= 0x39
        then Some(i+1)
        else None

let match_hexnum_char s i =
    if i >= String.length s
    then None
    else
        let code = Char.code s.[i] in
        if (code >= 0x30 && code <= 0x39) || (code >= 0x41 && code <= 0x46) || (code >= 0x61 && code <= 0x66)
        then Some(i+1)
        else None

let match_char c s i =
    if i >= String.length s
    then None
    else
        if c = s.[i]
        then Some(i+1)
        else None

let match_str pat s i =
    if (i + String.length pat) > String.length s
    then None
    else
        if  pat  = String.sub s i (String.length pat)
        then Some(i+String.length pat)
        else None

let opt pat s =
    let rec f acc i = match (acc, pat s i) with
        | (_, Some res) -> f (Some res) (i+1)
        | (before, None) -> before
    in f None 

let match_strlit s i =
    let rec f i =
        if i >= String.length s
        then None
        else if (i+1 < String.length s) && s.[i] = '\\' && s.[i+1] = '\"'
        then f (i+2)
        else if s.[i] = '"'
        then Some(i+1)
        else f (i+1)
    in
    if i+1 >= String.length s
    then None
    else if s.[i] = '"'
    then f (i+1)
    else None

let chain pat1 pat2 s i =
    match pat1 s i with
    | Some i -> pat2 s i
    | None -> None

let match_space = opt match_space_char
let match_ident = opt match_alph_char
let match_int = opt match_num_char
let match_hexint = chain (match_str "0x") (opt match_hexnum_char)

let update_pos s (fname, line, col, _) i =
    (fname, line, col, i)

exception LexException of pos_t

let rec lex s pos =
    let (_, _, _, i) = pos in
    let take e = String.sub s i (e-i) in
    if i = String.length s
    then [Parser.Eof]
    else
        match match_strlit s i with
        | Some i ->
            let inner = take i in
            Parser.Str (String.sub inner 1 ((String.length inner) - 2)) :: lex s (update_pos s pos i)
        | None -> match match_space s i with
        | Some i -> lex s (update_pos s pos i)
        | None -> match match_hexint s i with
        | Some i ->
            Parser.Int (int_of_string @@ take i) :: lex s (update_pos s pos i)
        | None -> match match_int s i with
        | Some i ->
            Parser.Int (int_of_string @@ take i) :: lex s (update_pos s pos i)

        (* 愚直すぎ (末尾再帰の最適化を狙っています。許して) *)
        | None -> match match_str "::" s i with
        | Some i -> Parser.Cons :: lex s (update_pos s pos i)
        | None -> match match_str "->" s i with
        | Some i -> Parser.Arrow :: lex s (update_pos s pos i)
        | None -> match match_str "+" s i with
        | Some i -> Parser.Add :: lex s (update_pos s pos i)
        | None -> match match_str "-" s i with
        | Some i -> Parser.Sub :: lex s (update_pos s pos i)
        | None -> match match_str "*" s i with
        | Some i -> Parser.Mul :: lex s (update_pos s pos i)
        | None -> match match_str "/" s i with
        | Some i -> Parser.Div :: lex s (update_pos s pos i)
        | None -> match match_str "||" s i with
        | Some i -> Parser.Or :: lex s (update_pos s pos i)
        | None -> match match_str "&&" s i with
        | Some i -> Parser.And :: lex s (update_pos s pos i)
        | None -> match match_str "=" s i with
        | Some i -> Parser.Eq :: lex s (update_pos s pos i)
        | None -> match match_str "<>" s i with
        | Some i -> Parser.Neq :: lex s (update_pos s pos i)
        | None -> match match_str ">" s i with
        | Some i -> Parser.Gret :: lex s (update_pos s pos i)
        | None -> match match_str "<" s i with
        | Some i -> Parser.Less :: lex s (update_pos s pos i)
        | None -> match match_str "(" s i with
        | Some i -> Parser.LP :: lex s (update_pos s pos i)
        | None -> match match_str ")" s i with
        | Some i -> Parser.RP :: lex s (update_pos s pos i)
        | None -> match match_str "[" s i with
        | Some i -> Parser.LB :: lex s (update_pos s pos i)
        | None -> match match_str "]" s i with
        | Some i -> Parser.RB :: lex s (update_pos s pos i)
        | None -> match match_str ";" s i with
        | Some i -> Parser.Semicol :: lex s (update_pos s pos i)
        | None -> match match_str "," s i with
        | Some i -> Parser.Comma :: lex s (update_pos s pos i)
        | None -> match match_str "|" s i with
        | Some i -> Parser.VBar :: lex s (update_pos s pos i)
        | None -> match match_ident s i with
        | Some i -> begin match take i with
            | "true" -> Parser.True :: lex s (update_pos s pos i)
            | "false" -> Parser.False :: lex s (update_pos s pos i)
            | "if" -> Parser.If :: lex s (update_pos s pos i)
            | "then" -> Parser.Then :: lex s (update_pos s pos i)
            | "else" -> Parser.Else :: lex s (update_pos s pos i)
            | "let" -> Parser.Let :: lex s (update_pos s pos i)
            | "rec" -> Parser.Rec :: lex s (update_pos s pos i)
            | "in" -> Parser.In :: lex s (update_pos s pos i)
            | "fun" -> Parser.Fun :: lex s (update_pos s pos i)
            | "match" -> Parser.Match :: lex s (update_pos s pos i)
            | "with" -> Parser.With :: lex s (update_pos s pos i)
            | "builtin" -> Parser.Builtin :: lex s (update_pos s pos i)
            | "mod" -> Parser.Mod :: lex s (update_pos s pos i)
            | "not" -> Parser.Not :: lex s (update_pos s pos i)
            | "ref" -> Parser.Ref :: lex s (update_pos s pos i)
            | ident -> Parser.Ident ident :: lex s (update_pos s pos i)
        end
        | None -> raise (LexException pos)
