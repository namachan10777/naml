type inst_t =
    | Ldi of int
    | Ldb of bool
    | Access of int
    | Closure of code_t
    | Let
    | EndLet
    | Test of code_t * code_t
    | And
    | Eq
    | App
    | TailApp
    | PushMark
    | Grab
    | Return
and code_t = inst_t list

type val_t =
    | Int of int
    | Bool of bool
    | Str of string
    | ClosVal of code_t * env_t
    | Eps
and env_t = val_t list
and stack_t = val_t list

type zam_t ={
    code: code_t;
    env: env_t;
    astack: stack_t;
    rstack: stack_t;
}
