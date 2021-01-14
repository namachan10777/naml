type exp_t =
    | Var of string
    | IntLit of int
    | BoolLit of bool
    | If of exp_t * exp_t * exp_t
    | Let of string * exp_t * exp_t
    | LetRec of string * string * exp_t * exp_t
    | Fun of string * exp_t
    | App of exp_t * exp_t
    | Eq of exp_t * exp_t
    | Neq of exp_t * exp_t
    | Greater of exp_t * exp_t
    | Less of exp_t * exp_t
    | Plus of exp_t * exp_t
    | Minus of exp_t * exp_t
    | Times of exp_t * exp_t
    | And of exp_t * exp_t
    | Or of exp_t * exp_t
    | Not of exp_t
    | Div of exp_t * exp_t
    | Empty
    | Match of exp_t * ((exp_t * exp_t) list)
    | Cons of exp_t * exp_t
    | Head of exp_t
    | Tail of exp_t
[@@deriving show]

type value_t =
    | IntVal of int
    | BoolVal of bool
    | ListVal of value_t list
    | FunVal of string * exp_t * env_t
    | RecFunVal of string * string * exp_t * env_t
    | RecFunVal2 of string * string * exp_t * env_t ref
[@@deriving show]

and env_t = (string * value_t) list
[@@deriving show]

let emptyenv () = []

let ext env x v = (x,v) :: env

let rec lookup x env =
   match env with | [] -> failwith ("unbound variable: " ^ x)
   | (y,v)::tl -> if x=y then v 
                  else lookup x tl 

let rec eval env =
    (* Plus, Times, Minus向け *)
    let binop_ii f lhr rhr = match (eval env lhr, eval env rhr) with
        | (IntVal lhr, IntVal rhr) -> IntVal (f lhr rhr)
        | _ -> failwith "integer type expected"
    in
    (* Less, Greater向け *)
    let binop_ib f lhr rhr = match (eval env lhr, eval env rhr) with
        | (IntVal lhr, IntVal rhr) -> BoolVal (f lhr rhr)
        | _ -> failwith "integer type expected"
    in
    (* And, Or向け *)
    let binop_bb f lhr rhr = match (eval env lhr, eval env rhr) with
        | (BoolVal lhr, BoolVal rhr) -> BoolVal (f lhr rhr)
        | _ -> failwith "boolean type expected"
    in
    function
    | IntLit n -> IntVal n
    | BoolLit b -> BoolVal b
    | Plus (lhr, rhr) -> binop_ii (+) lhr rhr
    | Minus (lhr, rhr) -> binop_ii (-) lhr rhr
    | Times (lhr, rhr) -> binop_ii ( * ) lhr rhr
    (* Divは分母が0の場合の処理を入れる必要があるので使えない *)
    | Div (lhr, rhr) -> begin match (eval env lhr, eval env rhr) with
        | (IntVal _, IntVal 0) -> failwith "divided by 0"
        | (IntVal lhr, IntVal rhr) -> IntVal(lhr / rhr)
        | _ -> failwith "integer value expected"
    end
    | Greater (lhr, rhr) -> binop_ib (>) lhr rhr
    | Less (lhr, rhr) -> binop_ib (<) lhr rhr
    | And (lhr, rhr) -> binop_bb (&&) lhr rhr
    | Or (lhr, rhr) -> binop_bb (||) lhr rhr
    | Not e -> begin match eval env e with
        | BoolVal b -> BoolVal (not b)
        | _ -> failwith "boolean type expected"
    end
    (* Eqはint、bool両方を受けるので使えない *)
    | Eq(lhr, rhr) -> begin match (eval env lhr, eval env rhr) with
        | (IntVal lhr, IntVal rhr) -> BoolVal (lhr = rhr)
        | (BoolVal lhr, BoolVal rhr) -> BoolVal (lhr = rhr)
        | _ -> failwith "integer type expected"
    end
    | Neq(lhr, rhr) -> begin match (eval env lhr, eval env rhr) with
        | (IntVal lhr, IntVal rhr) -> BoolVal (lhr != rhr)
        | (BoolVal lhr, BoolVal rhr) -> BoolVal (lhr != rhr)
        | _ -> BoolVal false
    end
    | If (cond, e1, e2) -> begin match eval env cond with
        | BoolVal true -> eval env e1
        | BoolVal false -> eval env e2
        | _ -> failwith "boolean type expected"
    end
    | Var(id) -> lookup id env
    | Let(id, e1, e2) -> eval (ext env id (eval env e1)) e2
    | _ -> raise @@ Failure "unimplemented!"
