open K7ast

let rec position x venv =
    match venv with
    | [] -> failwith "no matching variable in environment"
    | y :: venv -> if x = y then 0 else 1 + position x venv

let compile ast =
    let rec f venv = function
    | IntLit (i) -> [Cam.Ldi i]
    | BoolLit (b) -> [Cam.Ldb b]
    | StrLit s -> [Cam.Lds s]
    | Add (lhr, rhr)  -> binop venv Cam.Add  lhr rhr
    | Mul (lhr, rhr)  -> binop venv Cam.Mul  lhr rhr
    | Div (lhr, rhr)  -> binop venv Cam.Div  lhr rhr
    | Mod (lhr, rhr)  -> binop venv Cam.Mod  lhr rhr
    | Sub (lhr, rhr)  -> binop venv Cam.Sub  lhr rhr
    | Gret (lhr, rhr) -> binop venv Cam.Gret lhr rhr
    | Less (lhr, rhr) -> binop venv Cam.Less lhr rhr
    | Eq (lhr, rhr)   -> binop venv Cam.Eq   lhr rhr
    | Neq (lhr, rhr)  -> binop venv Cam.Neq  lhr rhr
    | Not e -> (f venv e) @ [Cam.Not]
    | Or (lhr, rhr)   ->
        (f venv lhr) @ [Cam.Test (
            [Ldb true],
            f venv rhr
        )]
    | And (lhr, rhr)  ->
        (f venv lhr) @ [Cam.Test (
            f venv rhr,
            [Ldb false]
        )]
    | If (cond, then_e, else_e) ->
        (f venv cond) @ [
            Test (
                f venv then_e,
                f venv else_e
            )
        ]
    | Let (id, def, expr) ->
        (f venv def) @ [Cam.Let] @ (f (id :: venv) expr) @ [Cam.EndLet]
    | Var id ->
        [Cam.Access (position id venv)]
    | Fun _ -> failwith "fun is unsupported"
    | Seq (lhr, rhr) -> (f venv lhr) @ (f venv rhr)
    | Match _ -> failwith "match is unsupported"
    | Emp -> failwith "emp is unsupported"
    | Cons _ -> failwith "cons is unsupported"
    | LetRec _ -> failwith "let rec is unsupported"
    | App _ -> failwith "app is unsupported"
    | Tuple _ -> failwith "tuple is unsupported"
    | Builtin _ -> failwith "builtin is unsupported"
    | DebugPrint _ -> failwith "debugprint is unsupported"
    and binop venv op lhr rhr =
        (f venv rhr) @ (f venv lhr) @ [op]
    in f ast
