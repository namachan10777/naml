open K7ast

let compile ast =
    let f _ = function
    | IntLit i -> [Zam.Ldi i]
    | BoolLit b -> [Zam.Ldb b]
    | StrLit s -> [Zam.Lds s]
    | Emp -> failwith "emp is unsupported"
    | Var _ -> failwith "var is unsupported"
    | If _ -> failwith "if is unsupported"
    | Let _ -> failwith "let is unsupported"
    | LetRec _ -> failwith "letrec is unsupported"
    | Fun _ -> failwith "fun is unsupported"
    | App _ -> failwith "app is unsupported"
    | Eq _ -> failwith "eq is unsupported"
    | Neq _ -> failwith "neq is unsupported"
    | Gret _ -> failwith "gret is unsupported"
    | Less _ -> failwith "less is unsupported"
    | Or _ -> failwith "or is unsupported"
    | And _ -> failwith "and is unsupported"
    | Add _ -> failwith "add is unsupported"
    | Sub _ -> failwith "sub is unsupported"
    | Mul _ -> failwith "mul is unsupported"
    | Div _ -> failwith "div is unsupported"
    | Mod _ -> failwith "mod is unsupported"
    | Not _ -> failwith "not is unsupported"
    | Match _ -> failwith "match is unsupported"
    | Tuple _ -> failwith "tuple is unsupported"
    | Cons _ -> failwith "cons is unsupported"
    | Builtin _ -> failwith "builtin is unsupported"
    | Seq _ -> failwith "seq is unsupported"
    | DebugPrint _ -> failwith "debugprint is unsupported"
    in { Zam.code = (f [] ast); Zam.astack = []; Zam.rstack = []; Zam.env = [] }
