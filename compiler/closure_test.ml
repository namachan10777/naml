let test a b =
    let ast = Closure.f @@ Typing.f @@ Alpha.f @@ Ast.f "test.ml" a in
    if ast = b then ()
    else (
      Printf.printf "left:\n%s\nright:\n%s" (Closure.show_inst_t ast)
        (Closure.show_inst_t b) ;
      failwith @@ "test failed: " ^ a )

module C = Closure
module T = Types

let () =
    test "let x = 1 + 2"
      [ C.LetInt (T.VidSSA 3, 2)
      ; C.LetInt (T.VidSSA 2, 1)
      ; C.LetCall (T.Vid 16, T.VidTop 0, [T.VidSSA 2; T.VidSSA 3])
      ; C.End ] ;
    test "let f x = x + 2"
      [ C.LetClosure
          ( T.Vid 16
          , [T.Vid 17]
          , [ C.LetInt (T.VidSSA 7, 2)
            ; C.LetCall (T.VidSSA 8, T.VidTop 0, [T.Vid 17; T.VidSSA 7]) ]
          , T.VidSSA 8
          , "_f_1"
          , [] )
      ; C.End ] ;
    test "let f x = let g y = x + y in g let x = f 1"
      [ C.LetClosure
          ( T.Vid 16
          , [T.Vid 17]
          , [ C.LetClosure
                ( T.Vid 18
                , [T.VidSSA 14; T.Vid 19]
                , [C.LetCall (T.VidSSA 15, T.VidTop 0, [T.VidSSA 14; T.Vid 19])]
                , T.VidSSA 15
                , "_f_1_g_2"
                , [T.Vid 17] ) ]
          , T.Vid 18
          , "_f_1"
          , [] )
      ; C.LetInt (T.VidSSA 10, 1)
      ; C.LetApp (T.Vid 20, T.Vid 16, [T.VidSSA 10])
      ; C.End ] ;
    test "let rec f x = f x"
      [ C.LetClosure
          ( T.Vid 16
          , [T.VidSSA 18; T.Vid 17]
          , [C.LetCall (T.VidSSA 19, T.VidSSA 18, [T.Vid 17])]
          , T.VidSSA 19
          , "_f_1"
          , [T.Vid 16] )
      ; C.End ] ;
    let src =
        "let rec fib n = if n = 0 || n = 1 then 1 else fib (n-1) + fib(n-2)"
    in
    match Closure.f @@ Typing.f @@ Alpha.f @@ Ast.f "test.ml" src with
    | [ C.LetClosure
          ( fib_id
          , [self_id; n_id]
          , [ C.LetInt (i0_id, 0)
            ; C.LetCall (eq1_id, T.VidTop 7, [n_id'; i0_id'])
            ; C.Test
                ( eq1_id'
                , [C.LetBool (eq_l_id, true)]
                , [ C.LetInt (i1_id, 1)
                  ; C.LetCall (eq_r_id, T.VidTop 7, [n_id''; i1_id']) ] )
            ; C.Phi (cmp_id, eq_l_id', eq_r_id')
            ; C.Test
                ( cmp_id'
                , [C.LetInt (fib_l_id, 1)]
                , [ C.LetInt (i1_2id, 2)
                  ; C.LetCall (nsub2_id, T.VidTop 1, [n_id3; i1_2id'])
                  ; C.LetCall (fibret_1id, self_id', [nsub2_id'])
                  ; C.LetInt (i1_1id, 1)
                  ; C.LetCall (nsub1_id, T.VidTop 1, [n_id4; i1_1id'])
                  ; C.LetCall (fibret_2id, self_id'', [nsub1_id'])
                  ; C.LetCall (fib_r_id, T.VidTop 0, [fibret_2id'; fibret_1id']) ]
                )
            ; C.Phi (ret_id, fib_l_id', fib_r_id') ]
          , ret_id'
          , "_fib_1"
          , [self_id'''] )
      ; C.End ]
      when n_id = n_id' && i0_id = i0_id' && eq_l_id = eq_l_id'
           && eq_r_id = eq_r_id' && fib_l_id = fib_l_id' && cmp_id' = cmp_id
           && i1_2id = i1_2id' && self_id = self_id' && i1_1id = i1_1id'
           && fib_id = self_id''' && n_id3 = n_id4 && n_id = n_id3
           && self_id = self_id'' && nsub1_id' = nsub1_id
           && nsub2_id = nsub2_id' && fibret_1id = fibret_1id'
           && fibret_2id = fibret_2id' && fib_r_id = fib_r_id'
           && ret_id = ret_id' && i1_id = i1_id' && n_id = n_id'' ->
        ()
    | invalid ->
        Printf.printf "%s\n" @@ Closure.show_inst_t invalid ;
        failwith "invalid ssa"
