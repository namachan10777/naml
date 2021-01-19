type pat_t =
    | Var of string
    | Cons of pat_t * pat_t
[@@deriving show]

(* LetRecもLetと同じ形にしておく *)
type exp_t =
    | Var of string
    | IntLit of int
    | BoolLit of bool
    | If of exp_t * exp_t * exp_t
    | Let of string * exp_t * exp_t
    | LetRec of string * exp_t * exp_t
    | Fun of string * exp_t
    | App of exp_t * exp_t
    | Eq of exp_t * exp_t
    | Neq of exp_t * exp_t
    | Gret of exp_t * exp_t
    | Less of exp_t * exp_t
    | Or of exp_t * exp_t
    | And of exp_t * exp_t
    | Add of exp_t * exp_t
    | Sub of exp_t * exp_t
    | Mul of exp_t * exp_t
    | Div of exp_t * exp_t
    | Emp of exp_t * exp_t
    | Match of exp_t * ((pat_t * exp_t) list)
    | Cons of exp_t * exp_t
    | Builtin of string
    | Seq of exp_t list
    | DebugPrint of exp_t
[@@deriving show]

(* RecFunValとFunValは区別しない（環境が変わるだけで同様に処理可能 *)
type value_t =
    | IntVal of int
    | BoolVal of bool
    | ListVal of value_t list
    | FunVal of string * exp_t * env_t ref
[@@deriving show]
and env_t = (string * value_t) list
[@@deriving show]

let emptyenv () = []
let ext env x v = (x, v) :: env
let rec lookup x env =
    match env with
    | [] -> failwith ("unbound variable: " ^ x)
    | (y, v) :: tl ->
        if x = y
        then v
        else lookup x tl

let eval _ = function
    | e -> failwith @@ Printf.sprintf "unsupported expression %s" @@ show_exp_t e
