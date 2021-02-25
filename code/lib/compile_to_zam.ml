open K7ast

let rec lookup x = function
    | y :: rest -> if x = y then 0 else 1 + lookup x rest
    | [] -> failwith @@ x ^ " is not found"

let compile ast =
    let rec f venv = function
        | IntLit i -> [Zam.Ldi i]
        | BoolLit b -> [Zam.Ldb b]
        | StrLit s -> [Zam.Lds s]
        | And (lhr, rhr) -> f venv lhr @ [Zam.Test (f venv rhr, [Zam.Ldb false])]
        | Or (lhr, rhr) -> f venv lhr @ [Zam.Test ([Zam.Ldb true], f venv rhr)]
        | Gret (lhr, rhr) -> f venv rhr @ f venv lhr @ [Zam.Gret]
        | Less (lhr, rhr) -> f venv rhr @ f venv lhr @ [Zam.Less]
        | Eq (lhr, rhr) -> f venv rhr @ f venv lhr @ [Zam.Eq]
        | Neq (lhr, rhr) -> f venv rhr @ f venv lhr @ [Zam.Neq]
        | Add (lhr, rhr) -> f venv rhr @ f venv lhr @ [Zam.Add]
        | Sub (lhr, rhr) -> f venv rhr @ f venv lhr @ [Zam.Sub]
        | Mul (lhr, rhr) -> f venv rhr @ f venv lhr @ [Zam.Mul]
        | Div (lhr, rhr) -> f venv rhr @ f venv lhr @ [Zam.Div]
        | Mod (lhr, rhr) -> f venv rhr @ f venv lhr @ [Zam.Mod]
        | Seq (lhr, rhr) -> f venv lhr @ [Zam.Drop] @ f venv rhr
        | Var id -> [Zam.Access (lookup id venv)]
        | If (cond, t, e) -> f venv cond @ [Zam.Test (f venv t, f venv e)]
        | Let (id, def, expr) ->
            f venv def @ [Zam.Let] @ f (id :: venv) expr @ [Zam.EndLet]
        | LetRec (id, Fun (arg, body), expr) ->
            [Zam.Closure (f (arg :: id :: venv) body @ [Zam.Return])]
            @ [Zam.Let]
            @ f (id :: venv) expr
            @ [Zam.EndLet]
        | LetRec _ -> failwith "let rec must be take function"
        | Fun _ as self ->
            let rec take_args = function
                | Fun (arg, body) -> let args, body = take_args body in arg :: args, body
                | body -> [], body
            in
            let args, body = take_args self in
            let args = List.rev args in
            let rec gen_args = function
                | arg :: args -> arg :: "" :: gen_args args
                | [] -> []
            in
            [Zam.Closure (List.init (List.length args - 1) (fun _ -> Zam.Grab) @ f (gen_args args @ venv) body @ [Zam.Return])]
        | App _ as self ->
            let rec take_args = function
                | App (g, arg) -> let g, args = take_args g in g, arg :: args
                | g -> g, []
            in
            let g, args = take_args self in
            Zam.PushMark :: (List.concat @@ List.map (f venv) args) @ f venv g @ [Zam.App]
        | Not e -> f venv e @ [Zam.Not]
        | Match _ -> failwith "match is unsupported"
        | Tuple _ -> failwith "tuple is unsupported"
        | Cons _ -> failwith "cons is unsupported"
        | Builtin _ -> failwith "builtin is unsupported"
        | DebugPrint _ -> failwith "debugprint is unsupported"
        | Emp -> failwith "emp is unsupported"
    in
    {Zam.code= f [] ast; Zam.astack= []; Zam.rstack= []; Zam.env= []}
