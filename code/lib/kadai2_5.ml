type exp_t =
    | IntLit of int
    | Plus of exp_t * exp_t
    | Times of exp_t * exp_t
    | BoolLit of bool
    | If of exp_t * exp_t * exp_t
    | Eq of exp_t * exp_t
    | Greater of exp_t * exp_t
    (* 課題外 *)
    | And of exp_t * exp_t
    | Or of exp_t * exp_t
    | Sub of exp_t * exp_t
    | Div of exp_t * exp_t
    | Not of exp_t
    | Less of exp_t * exp_t

type value_t =
    | IntVal of int
    | BoolVal of bool

let rec eval2 =
    function
    | IntLit n -> IntVal n
    | BoolLit b -> BoolVal b
    | Plus (lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (IntVal lhr, IntVal rhr) -> IntVal(lhr + rhr)
        | _ -> failwith "integer value expected"
    end
    | Sub (lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (IntVal lhr, IntVal rhr) -> IntVal(lhr + rhr)
        | _ -> failwith "integer value expected"
    end
    | Times (lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (IntVal lhr, IntVal rhr) -> IntVal(lhr * rhr)
        | _ -> failwith "integer value expected"
    end
    | Div (lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        (* 分母が0の場合をパターンマッチで先に除外 *)
        | (IntVal _, IntVal 0) -> failwith "divided by 0"
        | (IntVal lhr, IntVal rhr) -> IntVal(lhr / rhr)
        | _ -> failwith "integer value expected"
    end
    | Greater (lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (IntVal lhr, IntVal rhr) -> BoolVal(lhr > rhr)
        | _ -> failwith "integer value expected"
    end
    | Less (lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (IntVal lhr, IntVal rhr) -> BoolVal(lhr < rhr)
        | _ -> failwith "integer value expected"
    end
    | And (lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (BoolVal lhr, BoolVal rhr) -> BoolVal(lhr && rhr)
        | _ -> failwith "integer value expected"
    end
    | Or (lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (BoolVal lhr, BoolVal rhr) -> BoolVal(lhr || rhr)
        | _ -> failwith "integer value expected"
    end
    | Not e -> begin match eval2 e with
        | BoolVal b -> BoolVal (not b)
        | _ -> failwith "boolean type expected"
    end
    | Eq(lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (IntVal lhr, IntVal rhr) -> BoolVal (lhr = rhr)
        | (BoolVal lhr, BoolVal rhr) -> BoolVal (lhr = rhr)
        | _ -> failwith "integer type expected"
    end
    | If (cond, e1, e2) -> begin match eval2 cond with
        | BoolVal true -> eval2 e1
        | BoolVal false -> eval2 e2
        | _ -> failwith "boolean type expected"
    end

let rec eval2b =
    (* Plus, Times, Sub向け *)
    let binop_ii f lhr rhr = match (eval2b lhr, eval2b rhr) with
        | (IntVal lhr, IntVal rhr) -> IntVal (f lhr rhr)
        | _ -> failwith "integer type expected"
    in
    (* Less, Greater向け *)
    let binop_ib f lhr rhr = match (eval2b lhr, eval2b rhr) with
        | (IntVal lhr, IntVal rhr) -> BoolVal (f lhr rhr)
        | _ -> failwith "integer type expected"
    in
    (* And, Or向け *)
    let binop_bb f lhr rhr = match (eval2b lhr, eval2b rhr) with
        | (BoolVal lhr, BoolVal rhr) -> BoolVal (f lhr rhr)
        | _ -> failwith "boolean type expected"
    in
    function
    | IntLit n -> IntVal n
    | BoolLit b -> BoolVal b
    | Plus (lhr, rhr) -> binop_ii (+) lhr rhr
    | Sub (lhr, rhr) -> binop_ii (-) lhr rhr
    | Times (lhr, rhr) -> binop_ii ( * ) lhr rhr
    (* Divは分母が0の場合の処理を入れる必要があるので使えない *)
    | Div (lhr, rhr) -> begin match (eval2 lhr, eval2 rhr) with
        | (IntVal _, IntVal 0) -> failwith "divided by 0"
        | (IntVal lhr, IntVal rhr) -> IntVal(lhr / rhr)
        | _ -> failwith "integer value expected"
    end
    | Greater (lhr, rhr) -> binop_ib (>) lhr rhr
    | Less (lhr, rhr) -> binop_ib (<) lhr rhr
    | And (lhr, rhr) -> binop_bb (&&) lhr rhr
    | Or (lhr, rhr) -> binop_bb (||) lhr rhr
    | Not e -> begin match eval2b e with
        | BoolVal b -> BoolVal (not b)
        | _ -> failwith "boolean type expected"
    end
    (* Eqはint、bool両方を受けるので使えない *)
    | Eq(lhr, rhr) -> begin match (eval2b lhr, eval2b rhr) with
        | (IntVal lhr, IntVal rhr) -> BoolVal (lhr = rhr)
        | (BoolVal lhr, BoolVal rhr) -> BoolVal (lhr = rhr)
        | _ -> failwith "integer type expected"
    end
    | If (cond, e1, e2) -> begin match eval2b cond with
        | BoolVal true -> eval2b e1
        | BoolVal false -> eval2b e2
        | _ -> failwith "boolean type expected"
    end
