type pat_t =
    | PIntLit of int
    | PBoolLit of bool
    | PVar of string
    | PCons of pat_t * pat_t
    | PEmp
    | PTuple of pat_t list
[@@deriving show]

(* LetRecもLetと同じ形にしておく *)
type exp_t =
    | Var of string
    | IntLit of int
    | BoolLit of bool
    | StrLit of string
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
    | Mod of exp_t * exp_t
    | Not of exp_t
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
    | StrVal of string
    | ListVal of value_t list
    | FunVal of string * exp_t * env_t ref
    | BuiltinVal of string
    | TupleVal of value_t list
[@@deriving show]
and env_t = (string * value_t) list
[@@deriving show]

type stmt_t =
    | LetStmt of string * exp_t
[@@deriving show]

type ty_t =
    | TInt
    | TBool
    | TStr
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

type ctx_t = {
    env: env_t ref
}

let init_ctx () = {
    env = ref (emptyenv ());
}

let rec eval ctx =
    let { env } = ctx in
    let binop_int op lhr rhr =
        let rhr = eval ctx rhr in
        let lhr = eval ctx lhr in
        match (lhr, rhr) with
        | (IntVal lhr, IntVal rhr) -> IntVal (op lhr rhr)
        | _ -> failwith @@ Printf.sprintf "integer expected"
    in
    let binop_less_gret op lhr rhr =
        let rhr = eval ctx rhr in
        let lhr = eval ctx lhr in
        match (lhr, rhr) with
        | (IntVal lhr, IntVal rhr) -> BoolVal (op lhr rhr)
        | _ -> failwith @@ Printf.sprintf "integer expected"
    in
    let binop_bool op lhr rhr =
        let rhr = eval ctx rhr in
        let lhr = eval ctx lhr in
        match (lhr, rhr) with
        | (BoolVal lhr, BoolVal rhr) -> BoolVal (op lhr rhr)
        | _ -> failwith @@ Printf.sprintf "boolean expected"
    in
    let eq lhr rhr =
        let rhr = eval ctx rhr in
        let lhr = eval ctx lhr in
        match (lhr, rhr) with
        | (BoolVal lhr, BoolVal rhr) -> lhr = rhr
        | (IntVal lhr, IntVal rhr) -> lhr = rhr
        | (lhr, rhr) -> failwith @@ Printf.sprintf "cannot compare %s and %s" (show_value_t lhr) (show_value_t rhr)
    in
    function
    | Emp -> ListVal []
    | IntLit i -> IntVal i
    | BoolLit b -> BoolVal b
    | StrLit s -> StrVal s
    | Builtin builtin -> BuiltinVal builtin
    | Cons(e, next) -> begin match eval ctx next with
        | ListVal l -> ListVal ((eval ctx e) :: l)
        | _ -> failwith "list expected"
    end
    | Tuple tp -> TupleVal (List.map (eval ctx) tp)
    | Add(lhr, rhr) -> binop_int ( + ) lhr rhr
    | Sub(lhr, rhr) -> binop_int ( - ) lhr rhr
    | Mul(lhr, rhr) -> binop_int ( * ) lhr rhr
    | Div(lhr, rhr) -> binop_int ( / ) lhr rhr
    | Mod(lhr, rhr) -> binop_int ( mod ) lhr rhr
    | Gret(lhr, rhr) -> binop_less_gret ( > ) lhr rhr
    | Less(lhr, rhr) -> binop_less_gret ( < ) lhr rhr
    | And(lhr, rhr) -> binop_bool ( && ) lhr rhr
    | Or(lhr, rhr) -> binop_bool ( || ) lhr rhr
    | Not(v) -> begin match eval ctx v with
        | BoolVal b -> BoolVal (not b)
        | _ -> failwith @@ Printf.sprintf "boolean expected"
    end
    | Eq (lhr, rhr) -> BoolVal (eq lhr rhr)
    | Neq (lhr, rhr) -> BoolVal(not (eq lhr rhr))
    | Seq (lhr, rhr) ->
        eval ctx lhr |> ignore;
        eval ctx rhr
    | Var id -> lookup id !env
    | Fun (arg, expr) -> FunVal (arg, expr, env)
    | App (f, arg) ->
        let arg = eval ctx arg in
        let f = eval ctx f in
        begin match f with
        | FunVal (param, expr, env) ->
            let env = ext !env param arg in
            eval { env = ref env } expr
        | BuiltinVal "hd" -> begin match arg with
            | ListVal (h :: _) -> h
            | _ -> failwith "builtin hd: list length must be larger than 1"
            end
        | BuiltinVal "tl" -> begin match arg with
            | ListVal (_ :: tl) -> ListVal tl
            | _ -> failwith "builtin hd: list length must be larger than 1"
            end
        | _ -> failwith "function expected"
        end
    | Let(id, def, body) ->
        let env = ext !env id (eval ctx def) in
        eval { env = ref env }body
    (* 遅延して評価されるFunの中では自身を参照できるが、即時評価される式では自身を参照できない *)
    | LetRec(id, def, body) ->
        let env_ref = ref [] in
        env_ref := !env;
        let env = ext !env id (eval { env = env_ref } def) in
        env_ref := env;
        eval { env = env_ref } body
    | If (cond, exp_then, exp_else) ->
        begin match eval ctx cond with
        | BoolVal true -> eval ctx exp_then
        | BoolVal false -> eval ctx exp_else
        | _ -> failwith "if condition must be bool value"
        end
    | DebugPrint e ->
        let value = eval ctx e in
        print_endline (show_value_t value);
        value
    | Match(_, _) -> failwith @@ Printf.sprintf "match is unsupported"

let rec tcheck e =
    let check_bin_int name lhr rhr =
        match tcheck lhr, tcheck rhr with
        | (TInt, TInt) -> TInt
        | _ -> failwith @@ Printf.sprintf "type error in %s" name
    in
    let check_bin_cmp name lhr rhr =
        match tcheck lhr, tcheck rhr with
        | (TInt, TInt) -> TBool
        | _ -> failwith @@ Printf.sprintf "type error in %s" name
    in
    let check_bin_bool name lhr rhr =
        match tcheck lhr, tcheck rhr with
        | (TBool, TBool) -> TBool
        | _ -> failwith @@ Printf.sprintf "type error in %s" name
    in
    let check_bin_eq name lhr rhr =
        match tcheck lhr, tcheck rhr with
        | (TInt, TInt) -> TBool
        | (TBool, TBool) -> TBool
        | _ -> failwith @@ Printf.sprintf "type error in %s" name
    in
    match e with
    | IntLit _ -> TInt
    | BoolLit _ -> TBool
    | StrLit _ -> TStr
    | Add (lhr, rhr) -> check_bin_int "Add" lhr rhr
    | Sub (lhr, rhr) -> check_bin_int "Add" lhr rhr
    | Mul (lhr, rhr) -> check_bin_int "Add" lhr rhr
    | Div (lhr, rhr) -> check_bin_int "Add" lhr rhr
    | Mod (lhr, rhr) -> check_bin_int "Add" lhr rhr
    | Eq (lhr, rhr) -> check_bin_eq "Add" lhr rhr
    | Neq (lhr, rhr) -> check_bin_eq "Add" lhr rhr
    | Or (lhr, rhr) -> check_bin_bool "Add" lhr rhr
    | And (lhr, rhr) -> check_bin_bool "Add" lhr rhr
    | Not e -> begin match tcheck e with
        | TBool -> TBool
        | _ -> failwith "type error in Not"
    end
    | Gret (lhr, rhr) -> check_bin_cmp "Add" lhr rhr
    | Less (lhr, rhr) -> check_bin_cmp "Add" lhr rhr
    | If _ -> failwith "unsupported"
    | Let _ -> failwith "unsupported"
    | LetRec _ -> failwith "unsupported"
    | Fun _ -> failwith "unsupported"
    | App _ -> failwith "unsupported"
    | Emp -> failwith "unsupported"
    | Match _ -> failwith "unsupported"
    | Tuple _ -> failwith "unsupported"
    | Cons _ -> failwith "unsupported"
    | Builtin _ -> failwith "unsupported"
    | Seq _ -> failwith "unsupported"
    | DebugPrint _ -> failwith "unsupported"
    | Var _ -> failwith "unsupported"
