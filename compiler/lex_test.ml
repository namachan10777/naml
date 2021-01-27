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
let match_int = opt match_num_char
let match_hexint = chain (match_str "0x") (opt match_hexnum_char)

let update_pos s (fname, line, col, _) i =
    (fname, line, col, i)

exception LexException of pos_t

let rec lex s pos =
    let (_, _, _, i) = pos in
    let take e = String.sub s i (e-i) in
    if i = String.length s
    then [Lex.Eof]
    else
        match match_strlit s i with
        | Some i ->
            let inner = take i in
            Lex.Str (String.sub inner 1 ((String.length inner) - 2)) :: lex s (update_pos s pos i)
        | None -> match match_space s i with
        | Some i -> lex s (update_pos s pos i)
        | None -> match match_hexint s i with
        | Some i ->
            Lex.Int (int_of_string @@ take i) :: lex s (update_pos s pos i)
        | None -> match match_int s i with
        | Some i ->
            Lex.Int (int_of_string @@ take i) :: lex s (update_pos s pos i)

        (* 愚直すぎ (末尾再帰の最適化を狙っています。許して) *)
        | None -> match match_str "+" s i with
        | Some i -> Lex.Add :: lex s (update_pos s pos i)
        | None -> match match_str "-" s i with
        | Some i -> Lex.Sub :: lex s (update_pos s pos i)
        | None -> match match_str "*" s i with
        | Some i -> Lex.Mul :: lex s (update_pos s pos i)
        | None -> match match_str "/" s i with
        | Some i -> Lex.Div :: lex s (update_pos s pos i)
        | None -> match match_str "||" s i with
        | Some i -> Lex.Or :: lex s (update_pos s pos i)
        | None -> match match_str "&&" s i with
        | Some i -> Lex.And :: lex s (update_pos s pos i)
        | None -> match match_str "=" s i with
        | Some i -> Lex.Eq :: lex s (update_pos s pos i)
        | None -> match match_str "<>" s i with
        | Some i -> Lex.Neq :: lex s (update_pos s pos i)
        | None -> match match_str ">" s i with
        | Some i -> Lex.Gret :: lex s (update_pos s pos i)
        | None -> match match_str "<" s i with
        | Some i -> Lex.Cons :: lex s (update_pos s pos i)
        | None -> match match_str "(" s i with
        | Some i -> Lex.LP :: lex s (update_pos s pos i)
        | None -> match match_str ")" s i with
        | Some i -> Lex.RP :: lex s (update_pos s pos i)
        | None -> match match_str "[" s i with
        | Some i -> Lex.LB :: lex s (update_pos s pos i)
        | None -> match match_str "]" s i with
        | Some i -> Lex.RB :: lex s (update_pos s pos i)
        | None -> match match_str ";" s i with
        | Some i -> Lex.Semicol :: lex s (update_pos s pos i)
        | None -> match match_str "," s i with
        | Some i -> Lex.Comma :: lex s (update_pos s pos i)
        | None -> match match_str "|" s i with
        | Some i -> Lex.VBar :: lex s (update_pos s pos i)
        | None -> match match_str "->" s i with
        | Some i -> Lex.Arrow :: lex s (update_pos s pos i)
        | None -> match match_lower_ident s i with
        | Some i -> begin match take i with
            | "if" -> Lex.If :: lex s (update_pos s pos i)
            | "then" -> Lex.Then :: lex s (update_pos s pos i)
            | "else" -> Lex.Else :: lex s (update_pos s pos i)
            | "let" -> Lex.Let :: lex s (update_pos s pos i)
            | "rec" -> Lex.Rec :: lex s (update_pos s pos i)
            | "in" -> Lex.In :: lex s (update_pos s pos i)
            | "fun" -> Lex.Fun :: lex s (update_pos s pos i)
            | "match" -> Lex.Match :: lex s (update_pos s pos i)
            | "with" -> Lex.With :: lex s (update_pos s pos i)
            | "builtin" -> Lex.Builtin :: lex s (update_pos s pos i)
            | "mod" -> Lex.Mod :: lex s (update_pos s pos i)
            | "not" -> Lex.Not :: lex s (update_pos s pos i)
            | "ref" -> Lex.Ref :: lex s (update_pos s pos i)
            | ident -> Lex.LIdent ident :: lex s (update_pos s pos i)
        end
        | None -> raise (LexException pos)


let () =
    Test.assert_eq "count_newline 0 11" (count_newline "foo\nbar\nhoge" 0 11) 2;
    Test.assert_eq "count_newline 0 3" (count_newline "foo\nbar\nhoge" 0 3) 0;
    Test.assert_eq "count_newline 0 4" (count_newline "foo\nbar\nhoge" 0 4) 1;
    Test.assert_eq "count_newline 3 8" (count_newline "foo\nbar\nhoge" 3 8) 2;
    Test.assert_eq "match_space_char \" abc\" 0" (match_space_char " abc" 0) (Some 1);
    Test.assert_eq "match_space_char \" abc\" 1" (match_space_char " abc" 1) None;
    Test.assert_eq "match_space_char \"abc\" 0" (match_space_char "abc" 0) None;
    Test.assert_eq "match_space \" \\n\\r\\t\"" (match_space " \n\r\t" 0) (Some 4);
    Test.assert_eq "match_space \" \\n\\r\\t\" from 2" (match_space " \n\r\t" 2) (Some 4);
    Test.assert_eq "match_space \" \\n\\r\\thoge\" from 2 ends normal char" (match_space " \n\r\thoge" 2) (Some 4);
    Test.assert_eq "match_space \" \\n\\r\\thoge\" ends normal char" (match_space " \n\r\thoge" 0) (Some 4);
    Test.assert_eq "match_space \"hoo\"" (match_space "hoo" 0) None;
    Test.assert_eq "match_ident \"abc \"" (match_ident "abc " 0) (Some 3);
    Test.assert_eq "match_ident \" abc\"" (match_ident " abc" 0) None;
    Test.assert_eq "match_ident \"abc \"" (match_ident "abc " 0) (Some 3);
    Test.assert_eq "match_ident \" abc\"" (match_ident " abc" 0) None;
    Test.assert_eq "match_int \"123a\"" (match_int "123a" 0) (Some 3);
    Test.assert_eq "match_int \"a123\"" (match_int "a123" 0) None;
    Test.assert_eq "match_hexint \"0x123g\"" (match_hexint "0x123g" 0) (Some 5);
    Test.assert_eq "match_hexint \"a0x123\"" (match_hexint "a0x123" 0) None;
    Test.assert_eq "match_char '.' \".a\"" (match_char '.' ".a" 0) (Some 1);
    Test.assert_eq "match_char '.' \"a.\"" (match_char '.' "a." 0) None;
    Test.assert_eq "match_str \"hoge\" \"hoge\"" (match_str "hoge" "hoge" 0) (Some 4);
    Test.assert_eq "match_str \"hoge\" \"hog\"" (match_str "hoge" "hog" 0) None;
    Test.assert_eq "match_str \"hoge\" \"hogu\"" (match_str "hoge" "hogu" 0) None;
    Test.assert_eq "match_str \"hoge\" \" hoge\"" (match_str "hoge" " hoge" 1) (Some 5);
    Test.assert_eq "match_strlit \"\"hoge\"" (match_strlit "\"hoge\"" 0) (Some 6);
    Test.assert_eq "match_strlit \"\"ho\\\\ge\"" (match_strlit "\"ho\\\\ge\"" 0) (Some 8);
    Test.assert_eq "match_strlit \"\"ho\\\"ge\"" (match_strlit "\"ho\\\"ge\"" 0) (Some 8);
    Test.assert_eq "match_strlit \"\"hoge\"" (match_strlit "\"hoge" 0) None; 
    Test.assert_eq "match_strlit \"hoge\"\"" (match_strlit "hoge\"" 0) None; 
    Test.assert_eq "match_strlit \"\"\"\"" (match_strlit "\"\"" 0) (Some 2); 
    Test.assert_eq "lex strlit and space" (lex " \"hoge\" \"foo\"" (initial_pos "test.ml"))  [Lex.Str "hoge"; Lex.Str "foo"; Lex.Eof];
    Test.assert_eq "lex int and space" (lex "123 0xff" (initial_pos "test.ml"))  [Lex.Int 123; Lex.Int 255; Lex.Eof];
    Test.assert_eq "lex ident and space" (lex "aaa bbb" (initial_pos "test.ml"))  [Lex.LIdent "aaa"; Lex.LIdent "bbb"; Lex.Eof];
    Test.assert_eq "lex fib" (lex "let rec fib n = if n = 1 || n = 0 then 1 else (fib (n-1)) + fib (n-2) in fib 5"  (initial_pos "test.ml"))
        [
            Lex.Let; Lex.Rec; Lex.LIdent "fib"; Lex.LIdent "n"; Lex.Eq;
            Lex.If;
            Lex.LIdent "n"; Lex.Eq; Lex.Int 1;
            Lex.Or;
            Lex.LIdent "n"; Lex.Eq; Lex.Int 0;
            Lex.Then;
            Lex.Int 1;
            Lex.Else;
            Lex.LP; Lex.LIdent "fib"; Lex.LP; Lex.LIdent "n"; Lex.Sub; Lex.Int 1; Lex.RP; Lex.RP;
            Lex.Add;
            Lex.LIdent "fib"; Lex.LP; Lex.LIdent "n"; Lex.Sub; Lex.Int 2; Lex.RP;
            Lex.In;
            Lex.LIdent "fib";
            Lex.Int 5;
            Lex.Eof;
        ];
