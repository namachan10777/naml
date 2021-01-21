type cam_inst_t =
    (* Ldi(n)は、整数nをスタックに積む *)
    | Ldi of int
    (* Ldb(n)は、真理値bをスタックに積む *)
    | Ldb of bool
    (* Access(i)は環境i+1番目の値をスタックに積む *)
    | Access of int
    (* Clousure(c)は関数本体のコードがCで
     * その環境が、現在の環境であるような関数
     * クロージャを生成し、それをスタックに積む。
     * 前項で説明したように変数は名前の代わりに
     * 環境のインデックスで参照されるので、
     * このクロージャにも関数引数は含まれない
     * なお、この関数クロージャは再帰関数であるとして処理される
     *)
    | Closure of code_t
    (* スタックトップの値が関数クロージャならば、
     * その関数、スタックの上から二番目にある値に
     * 関数適用した計算を行う
     *)
    | Apply
    (* 関数の呼び出し元に戻る *)
    | Return
    (* スタックトップの値を環境の先頭に移す *)
    | Let
    (* 環境の先頭の値を取り除く *)
    | EndLet
    (* I_Test(c1, c2)はスタックトップの値がtrueならばコードc1を実行し、
     * falseならばコードを実行する
     *)
    | Test of code_t * code_t
    (* ここは分かって *)
    | Add
    | Eq
[@@deriving show]
and code_t = cam_inst_t list
[@@deriving show]

type value_t =
    | Int of int
    | Bool of bool
    | ClosureVal of code_t * value_t list
[@@deriving show]

type stack_t = value_t list
[@@deriving show]
type env_t = value_t list
[@@deriving show]

let exec insts =
    Printf.printf "-----------------------------\n";
    let rec exec env stack insts =
        Printf.printf "inst %s\n" @@ show_code_t insts;
        Printf.printf "stack %s\n" @@ show_stack_t stack;
        Printf.printf "env %s\n" @@ show_env_t env;
        match insts with 
        | [] -> List.hd stack
        | Ldi(n) :: rest -> exec env ((Int n) :: stack) rest
        | Ldb(b) :: rest -> exec env ((Bool b) :: stack) rest
        | Access(addr) :: rest -> exec env ((List.nth env addr) :: stack) rest
        | Closure(c) :: rest -> exec env ((ClosureVal (c, env)) :: stack) rest
        | Apply :: rest -> begin match stack with
            | ClosureVal (code, env') :: v :: stack -> exec (v :: ClosureVal (code, env') :: env') (ClosureVal (rest, env) :: stack) code
            | _ -> failwith @@ Printf.sprintf "cannot apply function %s" @@ show_stack_t stack
        end
        | Return :: _ -> begin match stack with
            | v :: ClosureVal (code, env') :: stack -> exec env' (v :: stack) code
            | _ -> failwith "cannot return"
        end
        | Let :: rest -> begin match stack with
            | v :: stack -> exec (v :: env) stack rest
            | _ -> failwith "cannot let"
        end
        | EndLet :: rest -> begin match env with
            | _ :: env -> exec env stack rest
            | _ -> failwith "cannot endlet"
        end
        | Test(c1, c2) :: rest -> begin match stack with
            | Bool true :: stack -> exec env stack (c1 @ rest)
            | Bool false :: stack -> exec env stack (c2 @ rest)
            | _ -> failwith "cannot test"
        end
        | Add :: rest -> begin match stack with
            | Int lhr :: Int rhr :: stack -> exec env (Int (lhr + rhr) :: stack) rest
            | _ -> failwith @@ Printf.sprintf "cannot add %s" @@ show_stack_t stack
        end
        | Eq :: rest -> begin match stack with
            | Int lhr :: Int rhr :: stack -> exec env (Bool (lhr = rhr) :: stack) rest
            | Bool lhr :: Bool rhr :: stack -> exec env (Bool (lhr = rhr) :: stack) rest
            | _ -> failwith "cannot add"
        end
    in exec [] [] insts
