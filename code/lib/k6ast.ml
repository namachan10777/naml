type pat_t =
    | PVar of string
    | PCons of pat_t * pat_t
    | PEmp
    | PTuple of pat_t * pat_t
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
    | Match of exp_t * ((pat_t * exp_t) list)
    | Emp
    | Tuple of exp_t list
    | Cons of exp_t * exp_t
    | Builtin of string
    | Seq of exp_t * exp_t
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

type stmt_t =
    | LetStmt of string * exp_t
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

let rec eval env =
    let binop_int op lhr rhr =
        let lhr = eval env lhr in
        let rhr = eval env rhr in
        match (lhr, rhr) with
        | (IntVal lhr, IntVal rhr) -> IntVal (op lhr rhr)
        | _ -> failwith @@ Printf.sprintf "integer expected"
    in
    function
    | IntLit i -> IntVal i
    | Add(lhr, rhr) -> binop_int ( + ) lhr rhr
    | Sub(lhr, rhr) -> binop_int ( - ) lhr rhr
    | Mul(lhr, rhr) -> binop_int ( * ) lhr rhr
    | Div(lhr, rhr) -> binop_int ( / ) lhr rhr
    | Let (id, def, expr) ->
        let env = (ext env id (eval env def)) in
        eval env expr
    | e -> failwith @@ Printf.sprintf "unsupported expression %s" @@ show_exp_t e
