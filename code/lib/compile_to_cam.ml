open K7ast

let compile ast =
    let rec f = function
    | IntLit (i) -> [Cam.Ldi i]
    | BoolLit (b) -> [Cam.Ldb b]
    | Add (lhr, rhr)  -> Cam.Add  :: (f lhr) @ (f rhr)
    | Mul (lhr, rhr)  -> Cam.Mul  :: (f lhr) @ (f rhr)
    | Div (lhr, rhr)  -> Cam.Div  :: (f lhr) @ (f rhr)
    | Sub (lhr, rhr)  -> Cam.Sub  :: (f lhr) @ (f rhr)
    | And (lhr, rhr)  -> Cam.And  :: (f lhr) @ (f rhr)
    | Or (lhr, rhr)   -> Cam.Or   :: (f lhr) @ (f rhr)
    | Gret (lhr, rhr) -> Cam.Gret :: (f lhr) @ (f rhr)
    | Less (lhr, rhr) -> Cam.Less :: (f lhr) @ (f rhr)
    | Eq (lhr, rhr)   -> Cam.Eq   :: (f lhr) @ (f rhr)
    | Neq (lhr, rhr)  -> Cam.Neq  :: (f lhr) @ (f rhr)
    | _ -> failwith "unsupported expr"
    in f ast |> List.rev
