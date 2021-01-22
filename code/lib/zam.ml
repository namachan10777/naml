type inst_t =
    | Ldi of int
    | Ldb of bool
    | Lds of string
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

let rec exec zam =
    match zam.code with
    | [] -> begin match zam with
        | { code = []; rstack = []; astack = [v]; env = [] } -> v
        | _ -> failwith "invalid state"
    end
    | Ldi i :: code -> exec { zam with code = code; astack = Int i :: zam.astack }
    | Ldb b :: code -> exec { zam with code = code; astack = Bool b :: zam.astack }
    | Lds s :: code -> exec { zam with code = code; astack = Str s :: zam.astack }
    | Access _ :: _ -> failwith "access is unsupported"
    | Closure _ :: _ -> failwith "closure is unsupported"
    | Let :: _ -> failwith "let is unsupported"
    | EndLet :: _ -> failwith "endlet is unsupported"
    | Test _ :: _ -> failwith "test is unsupported"
    | And :: _ -> failwith "and is unsupported"
    | Eq :: _ -> failwith "eq is unsupported"
    | App :: _ -> failwith "app is unsupported"
    | TailApp :: _ -> failwith "tailapp is unsupported"
    | PushMark :: _ -> failwith "pushmark is unsupported"
    | Grab :: _ -> failwith "grab is unsupported"
    | Return :: _ -> failwith "return is unsupported"
