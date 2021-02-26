let list_id = Id.from_strlist ["list"]

let ref_id = Id.from_strlist ["ref"]

let array_id = Id.from_strlist ["array"]

let vars =
    [ ( Id.from_strlist ["="]
      , (1, Types.Fun (Types.Poly 0, Types.Fun (Types.Poly 0, Types.Bool))) )
    ; ( Id.from_strlist ["<>"]
      , (1, Types.Fun (Types.Poly 0, Types.Fun (Types.Poly 0, Types.Bool))) )
    ; ( Id.from_strlist [";"]
      , (2, Types.Fun (Types.Poly 0, Types.Fun (Types.Poly 1, Types.Poly 1))) )
    ; ( Id.from_strlist ["+"]
      , (0, Types.Fun (Types.Int, Types.Fun (Types.Int, Types.Int))) )
    ; ( Id.from_strlist ["-"]
      , (0, Types.Fun (Types.Int, Types.Fun (Types.Int, Types.Int))) )
    ; (Id.from_strlist ["<neg>"], (0, Types.Fun (Types.Int, Types.Int)))
    ; (Id.from_strlist ["not"], (0, Types.Fun (Types.Bool, Types.Bool)))
    ; ( Id.from_strlist ["*"]
      , (0, Types.Fun (Types.Int, Types.Fun (Types.Int, Types.Int))) )
    ; ( Id.from_strlist ["/"]
      , (0, Types.Fun (Types.Int, Types.Fun (Types.Int, Types.Int))) )
    ; ( Id.from_strlist [">"]
      , (0, Types.Fun (Types.Int, Types.Fun (Types.Int, Types.Bool))) )
    ; ( Id.from_strlist ["<"]
      , (0, Types.Fun (Types.Int, Types.Fun (Types.Int, Types.Bool))) )
    ; ( Id.from_strlist ["mod"]
      , (0, Types.Fun (Types.Int, Types.Fun (Types.Int, Types.Int))) )
    ; ( Id.from_strlist ["||"]
      , (0, Types.Fun (Types.Bool, Types.Fun (Types.Bool, Types.Bool))) )
    ; ( Id.from_strlist ["&&"]
      , (0, Types.Fun (Types.Bool, Types.Fun (Types.Bool, Types.Bool))) )
    ; ( Id.from_strlist ["@"]
      , ( 1
        , Types.Fun
            ( Types.Variant ([Types.Poly 0], list_id)
            , Types.Fun
                ( Types.Variant ([Types.Poly 0], list_id)
                , Types.Variant ([Types.Poly 0], list_id) ) ) ) )
    ; ( Id.from_strlist ["<-"]
      , ( 1
        , Types.Fun
            ( Types.Variant ([Types.Poly 0], array_id)
            , Types.Fun (Types.Int, Types.Fun (Types.Poly 0, Types.unit)) ) ) )
    ; ( Id.from_strlist [":="]
      , ( 1
        , Types.Fun
            ( Types.Variant ([Types.Poly 0], array_id)
            , Types.Fun (Types.Poly 0, Types.unit) ) ) )
    ; ( Id.from_strlist ["."]
      , ( 1
        , Types.Fun
            ( Types.Variant ([Types.Poly 0], array_id)
            , Types.Fun (Types.Int, Types.Poly 0) ) ) )
    ; ( Id.from_strlist ["::"]
      , ( 1
        , Types.Fun
            ( Types.Poly 0
            , Types.Fun
                ( Types.Variant ([Types.Poly 0], list_id)
                , Types.Variant ([Types.Poly 0], list_id) ) ) ) )
    ; (Id.from_strlist ["print_string"], (0, Types.Fun (Types.Str, Types.unit)))
    ; (Id.from_strlist ["print_int"], (0, Types.Fun (Types.Int, Types.unit)))
    ; (Id.from_strlist ["failwith"], (1, Types.Fun (Types.Str, Types.Poly 0)))
    ]

let names = List.map fst vars