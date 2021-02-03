type exp_t =
    | IntLit of int
    | Plus of exp_t * exp_t
    | Times of exp_t * exp_t
    | BoolLit of bool
    | If of exp_t * exp_t * exp_t
    | Eq of exp_t * exp_t
    | Greater of exp_t * exp_t
    | Var of string
    | Let of string * exp_t * exp_t
    (* 課題外 *)
    | And of exp_t * exp_t
    | Or of exp_t * exp_t
    | Sub of exp_t * exp_t
    | Div of exp_t * exp_t
    | Not of exp_t
    | Less of exp_t * exp_t

type value_t = IntVal of int | BoolVal of bool [@@deriving show]

type env_t = (string * value_t) list [@@deriving show]

let emptyenv () = []

let ext env x v = (x, v) :: env

let rec lookup x env =
    match env with
    | [] -> failwith ("unbound variable: " ^ x)
    | (y, v) :: tl -> if x = y then v else lookup x tl

let rec eval3 env =
    let () = print_endline (show_env_t env) in
    (* Plus, Times, Sub向け *)
    let binop_ii f lhr rhr =
        match (eval3 env lhr, eval3 env rhr) with
        | IntVal lhr, IntVal rhr -> IntVal (f lhr rhr)
        | _ -> failwith "integer type expected"
    in
    (* Less, Greater向け *)
    let binop_ib f lhr rhr =
        match (eval3 env lhr, eval3 env rhr) with
        | IntVal lhr, IntVal rhr -> BoolVal (f lhr rhr)
        | _ -> failwith "integer type expected"
    in
    (* And, Or向け *)
    let binop_bb f lhr rhr =
        match (eval3 env lhr, eval3 env rhr) with
        | BoolVal lhr, BoolVal rhr -> BoolVal (f lhr rhr)
        | _ -> failwith "boolean type expected"
    in
    function
    | IntLit n -> IntVal n
    | BoolLit b -> BoolVal b
    | Plus (lhr, rhr) -> binop_ii ( + ) lhr rhr
    | Sub (lhr, rhr) -> binop_ii ( - ) lhr rhr
    | Times (lhr, rhr) -> binop_ii ( * ) lhr rhr
    (* Divは分母が0の場合の処理を入れる必要があるので使えない *)
    | Div (lhr, rhr) -> (
      match (eval3 env lhr, eval3 env rhr) with
      | IntVal _, IntVal 0 -> failwith "divided by 0"
      | IntVal lhr, IntVal rhr -> IntVal (lhr / rhr)
      | _ -> failwith "integer value expected" )
    | Greater (lhr, rhr) -> binop_ib ( > ) lhr rhr
    | Less (lhr, rhr) -> binop_ib ( < ) lhr rhr
    | And (lhr, rhr) -> binop_bb ( && ) lhr rhr
    | Or (lhr, rhr) -> binop_bb ( || ) lhr rhr
    | Not e -> (
      match eval3 env e with
      | BoolVal b -> BoolVal (not b)
      | _ -> failwith "boolean type expected" )
    (* Eqはint、bool両方を受けるので使えない *)
    | Eq (lhr, rhr) -> (
      match (eval3 env lhr, eval3 env rhr) with
      | IntVal lhr, IntVal rhr -> BoolVal (lhr = rhr)
      | BoolVal lhr, BoolVal rhr -> BoolVal (lhr = rhr)
      | _ -> failwith "integer type expected" )
    | If (cond, e1, e2) -> (
      match eval3 env cond with
      | BoolVal true -> eval3 env e1
      | BoolVal false -> eval3 env e2
      | _ -> failwith "boolean type expected" )
    | Var id -> lookup id env
    | Let (id, e1, e2) -> eval3 (ext env id (eval3 env e1)) e2
