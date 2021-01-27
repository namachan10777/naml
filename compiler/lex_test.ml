open Lex

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
    Test.assert_eq "match_lower_char \"a \"" (match_alph_lower_char "a " 0) (Some 1);
    Test.assert_eq "match_upper_char \"A \"" (match_alph_upper_char "A " 0) (Some 1);
    Test.assert_eq "match_lower_char \"A \"" (match_alph_lower_char "A " 0) None;
    Test.assert_eq "match_upper_char \"a \"" (match_alph_upper_char "a " 0) None;
    Test.assert_eq "match_lower_ident \" abc\"" (match_lower_ident " abc" 0) None;
    Test.assert_eq "match_lower_ident \"abc \"" (match_lower_ident "abc " 0) (Some 3);
    Test.assert_eq "match_lower_ident \" abc\"" (match_lower_ident " abc" 0) None;
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
    Test.assert_eq "f g 0" (lex "f g 0" (initial_pos "test.ml")) [Lex.LIdent "f"; Lex.LIdent "g"; Lex.Int 0; Lex.Eof];
    Test.assert_eq "lex strlit and space" (lex " \"hoge\" \"foo\"" (initial_pos "test.ml"))  [Lex.Str "hoge"; Lex.Str "foo"; Lex.Eof];
    Test.assert_eq "lex int and space" (lex "123 0xff" (initial_pos "test.ml"))  [Lex.Int 123; Lex.Int 255; Lex.Eof];
    Test.assert_eq "lex ident and space" (lex "aaa bbb" (initial_pos "test.ml"))  [Lex.LIdent "aaa"; Lex.LIdent "bbb"; Lex.Eof];
    try (lex "let rec fib n = if n = 1 || n = 0 then 1 else (fib (n-1)) + fib (n-2) in fib 5"  (initial_pos "test.ml")) |> ignore with
        | LexException p -> Printf.printf "%s\n" @@ Lex.show_pos_t p;
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
