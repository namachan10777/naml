type exp_t =
    | IntLit of int
    | Plus of exp_t * exp_t
    | Times of exp_t * exp_t
    | Sub of exp_t * exp_t
    | Div of exp_t * exp_t
    | BoolLit of bool
    | If of exp_t * exp_t * exp_t
    | Eq of exp_t * exp_t
    | And of exp_t * exp_t
    | Or of exp_t * exp_t
    | Not of exp_t
    | Greater of exp_t * exp_t
    | Less of exp_t * exp_t

type value_t =
    | IntVal of int
    | BoolVal of bool

let rec eval2 = function
    | IntLit n -> IntVal n
    | BoolLit b -> BoolVal b
    | Plus(lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (IntVal lhr, IntVal rhr) -> IntVal(lhr + rhr)
        | _ -> failwith "interger values expected"
    end
    | Sub(lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (IntVal lhr, IntVal rhr) -> IntVal(lhr - rhr)
        | _ -> failwith "interger values expected"
    end
    | Times(lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (IntVal lhr, IntVal rhr) -> IntVal(lhr * rhr)
        | _ -> failwith "interger values expected"
    end
    | Div(lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (IntVal _, IntVal 0) -> failwith "divided by 0"
        | (IntVal lhr, IntVal rhr) -> IntVal(lhr / rhr)
        | _ -> failwith "interger values expected"
    end
    | Greater(lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (IntVal lhr, IntVal rhr) -> BoolVal(lhr > rhr)
        | _ -> failwith "interger values expected"
    end
    | Less(lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (IntVal _, IntVal 0) -> failwith "divided by 0"
        | (IntVal lhr, IntVal rhr) -> BoolVal(lhr < rhr)
        | _ -> failwith "interger values expected"
    end
    | And(lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (BoolVal lhr, BoolVal rhr) -> BoolVal(lhr && rhr)
        | _ -> failwith "boolean values expected"
    end

    | Or(lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (BoolVal lhr, BoolVal rhr) -> BoolVal(lhr || rhr)
        | _ -> failwith "boolean values expected"
    end

    | Not e -> begin match eval2 e with
        | BoolVal b -> BoolVal(not b)
        | _ -> failwith "boolean values expected"
    end

    | Eq(lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (BoolVal lhr, BoolVal rhr) -> BoolVal(lhr = rhr)
        | (IntVal lhr, IntVal rhr) -> BoolVal(lhr = rhr)
        | _ -> failwith "wrong value"
    end

    | If(cond, e1, e2) -> begin match eval2 cond with
        | BoolVal true -> eval2 e1
        | BoolVal false -> eval2 e2
        | _ -> failwith "expected boolean value"
    end
