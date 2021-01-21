open K7ast

let compile ast =
    let rec f = function
    | IntLit (i) -> [Cam.Ldi i]
    | BoolLit (b) -> [Cam.Ldb b]
    | Add (lhr, rhr)  -> binop Cam.Add  lhr rhr
    | Mul (lhr, rhr)  -> binop Cam.Mul  lhr rhr
    | Div (lhr, rhr)  -> binop Cam.Div  lhr rhr
    | Sub (lhr, rhr)  -> binop Cam.Sub  lhr rhr
    | Gret (lhr, rhr) -> binop Cam.Gret lhr rhr
    | Less (lhr, rhr) -> binop Cam.Less lhr rhr
    | Eq (lhr, rhr)   -> binop Cam.Eq   lhr rhr
    | Neq (lhr, rhr)  -> binop Cam.Neq  lhr rhr
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
    | _ -> failwith "unsupported expr"
    and binop op lhr rhr =
        (f rhr) @ (f lhr) @ [op]
    in f ast
