open K7ast

let compile ast =
    let rec f = function
    | IntLit (i) -> [Cam.Ldi i]
    | BoolLit (b) -> [Cam.Ldb b]
    | Add (lhr, rhr)  -> binop Cam.Add  lhr rhr
    | Mul (lhr, rhr)  -> binop Cam.Mul  lhr rhr
    | Div (lhr, rhr)  -> binop Cam.Div  lhr rhr
    | Mod (lhr, rhr)  -> binop Cam.Mod  lhr rhr
    | Sub (lhr, rhr)  -> binop Cam.Sub  lhr rhr
    | Gret (lhr, rhr) -> binop Cam.Gret lhr rhr
    | Less (lhr, rhr) -> binop Cam.Less lhr rhr
    | Eq (lhr, rhr)   -> binop Cam.Eq   lhr rhr
    | Neq (lhr, rhr)  -> binop Cam.Neq  lhr rhr
    | Not e -> (f e) @ [Cam.Not]
    | Or (lhr, rhr)   ->
        (f lhr) @ [Cam.Test (
            [Ldb true],
            f rhr
        )]
    | And (lhr, rhr)  ->
        (f lhr) @ [Cam.Test (
            f rhr,
            [Ldb false]
        )]
    | If (cond, then_e, else_e) ->
        (f cond) @ [
            Test (
                f then_e,
                f else_e
            )
        ]
    | Match _ -> failwith "match is unsupported"
    | Emp -> failwith "emp is unsupported"
    | Cons _ -> failwith "cons is unsupported"
    | Var _ -> failwith "var is unsupported"
    | StrLit _ -> failwith "strlit is unsupported"
    | Let _ -> failwith "let is unsupported"
    | LetRec _ -> failwith "let rec is unsupported"
    | Fun _ -> failwith "fun is unsupported"
    | App _ -> failwith "app is unsupported"
    | Tuple _ -> failwith "tuple is unsupported"
    | Builtin _ -> failwith "builtin is unsupported"
    | Seq _ -> failwith "seq is unsupported"
    | DebugPrint _ -> failwith "debugprint is unsupported"
    and binop op lhr rhr =
        (f rhr) @ (f lhr) @ [op]
    in f ast
