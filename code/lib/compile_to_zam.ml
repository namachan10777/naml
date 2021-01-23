open K7ast

let compile ast =
    let rec f venv = function
    | IntLit i -> [Zam.Ldi i]
    | BoolLit b -> [Zam.Ldb b]
    | StrLit s -> [Zam.Lds s]
    | And (lhr, rhr)  ->
        (f venv lhr) @ [Zam.Test (f venv rhr, [Zam.Ldb false])]
    | Or (lhr, rhr)   ->
        (f venv lhr) @ [Zam.Test ([Zam.Ldb true], f venv rhr)]
    | Gret (lhr, rhr) -> (f venv rhr) @ (f venv lhr) @ [Zam.Gret]
    | Less (lhr, rhr) -> (f venv rhr) @ (f venv lhr) @ [Zam.Less]
    | Eq (lhr, rhr)   -> (f venv rhr) @ (f venv lhr) @ [Zam.Eq]
    | Neq (lhr, rhr)  -> (f venv rhr) @ (f venv lhr) @ [Zam.Neq]
    | Add (lhr, rhr)  -> (f venv rhr) @ (f venv lhr) @ [Zam.Add]
    | Sub (lhr, rhr)  -> (f venv rhr) @ (f venv lhr) @ [Zam.Sub]
    | Mul (lhr, rhr)  -> (f venv rhr) @ (f venv lhr) @ [Zam.Mul]
    | Div (lhr, rhr)  -> (f venv rhr) @ (f venv lhr) @ [Zam.Div]
    | Mod (lhr, rhr)  -> (f venv rhr) @ (f venv lhr) @ [Zam.Mod]
    | Emp -> failwith "emp is unsupported"
    | Var _ -> failwith "var is unsupported"
    | If _ -> failwith "if is unsupported"
    | Let _ -> failwith "let is unsupported"
    | LetRec _ -> failwith "letrec is unsupported"
    | Fun _ -> failwith "fun is unsupported"
    | App _ -> failwith "app is unsupported"
    | Not _ -> failwith "not is unsupported"
    | Match _ -> failwith "match is unsupported"
    | Tuple _ -> failwith "tuple is unsupported"
    | Cons _ -> failwith "cons is unsupported"
    | Builtin _ -> failwith "builtin is unsupported"
    | Seq _ -> failwith "seq is unsupported"
    | DebugPrint _ -> failwith "debugprint is unsupported"
    in { Zam.code = (f [] ast); Zam.astack = []; Zam.rstack = []; Zam.env = [] }
