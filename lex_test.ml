open Lex

type t = (Lex.t * Lex.pos_t) list

let assert_eq name a b =
    if a = b then () else failwith @@ Printf.sprintf "test failed %s" name

let () =
    assert_eq "count_newline 0 11" (count_newline "foo\nbar\nhoge" 0 11) 2 ;
    assert_eq "count_newline 0 3" (count_newline "foo\nbar\nhoge" 0 3) 0 ;
    assert_eq "count_newline 0 4" (count_newline "foo\nbar\nhoge" 0 4) 1 ;
    assert_eq "count_newline 3 8" (count_newline "foo\nbar\nhoge" 3 8) 2 ;
    assert_eq "match_space_char \" abc\" 0" (match_space_char " abc" 0) (Some 1) ;
    assert_eq "match_space_char \" abc\" 1" (match_space_char " abc" 1) None ;
    assert_eq "match_space_char \"abc\" 0" (match_space_char "abc" 0) None ;
    assert_eq "match_space \" \\n\\r\\t\"" (match_space " \n\r\t" 0) (Some 4) ;
    assert_eq "match_space \" \\n\\r\\t\" from 2" (match_space " \n\r\t" 2)
      (Some 4) ;
    assert_eq "match_space \" \\n\\r\\thoge\" from 2 ends normal char"
      (match_space " \n\r\thoge" 2)
      (Some 4) ;
    assert_eq "match_space \" \\n\\r\\thoge\" ends normal char"
      (match_space " \n\r\thoge" 0)
      (Some 4) ;
    assert_eq "match_space \"hoo\"" (match_space "hoo" 0) None ;
    assert_eq "match_lower_char \"a \"" (match_alph_lower_char "a " 0) (Some 1) ;
    assert_eq "match_upper_char \"A \"" (match_alph_upper_char "A " 0) (Some 1) ;
    assert_eq "match_lower_char \"A \"" (match_alph_lower_char "A " 0) None ;
    assert_eq "match_upper_char \"a \"" (match_alph_upper_char "a " 0) None ;
    assert_eq "match_lower_ident \" abc\"" (match_lower_ident " abc" 0) None ;
    assert_eq "match_lower_ident \"abc \"" (match_lower_ident "abc " 0) (Some 3) ;
    assert_eq "match_lower_ident \" abc\"" (match_lower_ident " abc" 0) None ;
    assert_eq "match_int \"123a\"" (match_int "123a" 0) (Some 3) ;
    assert_eq "match_int \"a123\"" (match_int "a123" 0) None ;
    assert_eq "match_hexint \"0x123g\"" (match_hexint "0x123g" 0) (Some 5) ;
    assert_eq "match_hexint \"a0x123\"" (match_hexint "a0x123" 0) None ;
    assert_eq "match_char '.' \".a\"" (match_char '.' ".a" 0) (Some 1) ;
    assert_eq "match_char '.' \"a.\"" (match_char '.' "a." 0) None ;
    assert_eq "match_str \"hoge\" \"hoge\"" (match_str "hoge" "hoge" 0) (Some 4) ;
    assert_eq "match_str \"hoge\" \"hog\"" (match_str "hoge" "hog" 0) None ;
    assert_eq "match_str \"hoge\" \"hogu\"" (match_str "hoge" "hogu" 0) None ;
    assert_eq "match_str \"hoge\" \" hoge\""
      (match_str "hoge" " hoge" 1)
      (Some 5) ;
    assert_eq "match_strlit \"\"hoge\"" (match_strlit "\"hoge\"" 0) (Some 6) ;
    assert_eq "match_strlit \"\"ho\\\\ge\""
      (match_strlit "\"ho\\\\ge\"" 0)
      (Some 8) ;
    assert_eq "match_strlit \"\"ho\\\"ge\""
      (match_strlit "\"ho\\\"ge\"" 0)
      (Some 8) ;
    assert_eq "match_strlit \"\"hoge\"" (match_strlit "\"hoge" 0) None ;
    assert_eq "match_strlit \"hoge\"\"" (match_strlit "hoge\"" 0) None ;
    assert_eq "match_strlit \"\"\"\"" (match_strlit "\"\"" 0) (Some 2) ;
    assert_eq "match_charlit 'a'" (match_charlit "'a'" 0) (Some 3) ;
    assert_eq "match_charlit '\\\''" (match_charlit "'\\\''" 0) (Some 4) ;
    assert_eq "update 1"
      (update_pos "hoge" (initial_pos "test.ml") 4)
      ("test.ml", 1, 4, 4) ;
    assert_eq "update 2"
      (update_pos "ho\nge" (initial_pos "test.ml") 5)
      ("test.ml", 2, 2, 5) ;
    assert_eq "update 1"
      (update_pos "hoge\n" (initial_pos "test.ml") 5)
      ("test.ml", 2, 0, 5) ;
    assert_eq "update 1"
      (update_pos "\nhoge" (initial_pos "test.ml") 5)
      ("test.ml", 2, 4, 5) ;
    assert_eq "f g 0"
      (lex "f g 0" (initial_pos ""))
      [ (Lex.LIdent "f", ("", 1, 1, 1))
      ; (Lex.LIdent "g", ("", 1, 3, 3))
      ; (Lex.Int 0, ("", 1, 5, 5))
      ; (Lex.Eof, ("", 1, 5, 5)) ] ;
    assert_eq "lex strlit and space"
      (lex " \"hoge\" \"foo\"" (initial_pos ""))
      [ (Lex.Str "hoge", ("", 1, 2, 2))
      ; (Lex.Str "foo", ("", 1, 9, 9))
      ; (Lex.Eof, ("", 1, 13, 13)) ] ;
    assert_eq "lex int and space"
      (lex "123 0xff" (initial_pos ""))
      [ (Lex.Int 123, ("", 1, 1, 1))
      ; (Lex.Int 255, ("", 1, 5, 5))
      ; (Lex.Eof, ("", 1, 8, 8)) ] ;
    assert_eq "lex ident and space"
      (lex "aaa bbb" (initial_pos ""))
      [ (Lex.LIdent "aaa", ("", 1, 1, 1))
      ; (Lex.LIdent "bbb", ("", 1, 5, 5))
      ; (Lex.Eof, ("", 1, 7, 7)) ] ;
    assert_eq "'a' '\\''"
      (lex "'a' '\\''" (initial_pos ""))
      [ (Lex.Char 'a', ("", 1, 1, 1))
      ; (Lex.Char '\'', ("", 1, 5, 5))
      ; (Lex.Eof, ("", 1, 8, 8)) ] ;
    assert_eq "tvar 'a "
      (lex "'a" (initial_pos ""))
      [(Lex.TVar "a", ("", 1, 1, 1)); (Lex.Eof, ("", 1, 2, 2))] ;
    assert_eq "lex [x]"
      (lex "[x]" (initial_pos ""))
      [ (Lex.LB, ("", 1, 1, 1))
      ; (Lex.LIdent "x", ("", 1, 2, 2))
      ; (Lex.RB, ("", 1, 3, 3))
      ; (Lex.Eof, ("", 1, 3, 3)) ] ;
    ( try
        lex
          "let rec fib n = if n = 1 || n = 0 then 1 else (fib (n-1)) + fib \
           (n-2) in fib 5"
          (initial_pos "test.ml")
        |> ignore
      with LexException p -> Printf.printf "%s\n" @@ Lex.show_pos_t p ) ;
    assert_eq "lex fib"
      (lex
         "let rec fib n = if n = 1 || n = 0 then 1 else (fib (n-1)) + fib (n-2) \n\
          in fib 5" (initial_pos ""))
      [ (Lex.Let, ("", 1, 1, 1))
      ; (Lex.Rec, ("", 1, 5, 5))
      ; (Lex.LIdent "fib", ("", 1, 9, 9))
      ; (Lex.LIdent "n", ("", 1, 13, 13))
      ; (Lex.Eq, ("", 1, 15, 15))
      ; (Lex.If, ("", 1, 17, 17))
      ; (Lex.LIdent "n", ("", 1, 20, 20))
      ; (Lex.Eq, ("", 1, 22, 22))
      ; (Lex.Int 1, ("", 1, 24, 24))
      ; (Lex.Or, ("", 1, 26, 26))
      ; (Lex.LIdent "n", ("", 1, 29, 29))
      ; (Lex.Eq, ("", 1, 31, 31))
      ; (Lex.Int 0, ("", 1, 33, 33))
      ; (Lex.Then, ("", 1, 35, 35))
      ; (Lex.Int 1, ("", 1, 40, 40))
      ; (Lex.Else, ("", 1, 42, 42))
      ; (Lex.LP, ("", 1, 47, 47))
      ; (Lex.LIdent "fib", ("", 1, 48, 48))
      ; (Lex.LP, ("", 1, 52, 52))
      ; (Lex.LIdent "n", ("", 1, 53, 53))
      ; (Lex.Sub, ("", 1, 54, 54))
      ; (Lex.Int 1, ("", 1, 55, 55))
      ; (Lex.RP, ("", 1, 56, 56))
      ; (Lex.RP, ("", 1, 57, 57))
      ; (Lex.Add, ("", 1, 59, 59))
      ; (Lex.LIdent "fib", ("", 1, 61, 61))
      ; (Lex.LP, ("", 1, 65, 65))
      ; (Lex.LIdent "n", ("", 1, 66, 66))
      ; (Lex.Sub, ("", 1, 67, 67))
      ; (Lex.Int 2, ("", 1, 68, 68))
      ; (Lex.RP, ("", 1, 69, 69))
      ; (Lex.In, ("", 2, 1, 72))
      ; (Lex.LIdent "fib", ("", 2, 4, 75))
      ; (Lex.Int 5, ("", 2, 8, 79))
      ; (Lex.Eof, ("", 2, 8, 79)) ]
