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

let rec eval2b =
    let binop_ii f lhr rhr = match (eval2b lhr, eval2b rhr) with
        | (IntVal lhr, IntVal rhr) -> IntVal (f lhr rhr)
        | _ -> failwith "integer type expected"
    in
    let binop_ib f lhr rhr = match (eval2b lhr, eval2b rhr) with
        | (IntVal lhr, IntVal rhr) -> BoolVal (f lhr rhr)
        | _ -> failwith "integer type expected"
    in
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
    | Div (lhr, rhr) -> binop_ii (/) lhr rhr
    | Greater (lhr, rhr) -> binop_ib (>) lhr rhr
    | Less (lhr, rhr) -> binop_ib (<) lhr rhr
    | And (lhr, rhr) -> binop_bb (&&) lhr rhr
    | Or (lhr, rhr) -> binop_bb (||) lhr rhr
    | Not e -> begin match eval2b e with
        | BoolVal b -> BoolVal (not b)
        | _ -> failwith "boolean type expected"
    end
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
