(* 課題2-2 *)
type exp =
    | IntLit of int
    | Plus of exp * exp
    | Times of exp * exp
    (* 減算と除算の追加 *)
    | Subtract of exp * exp
    | Divide of exp * exp

(* Eというexp型の式を受けてPlus(e, IntLit 2)を返す関数 *)
let f e = Plus (e, IntLit 2)

(* 整数リテラルを全てその絶対値の整数リテラルで置き換える関数 *)
let rec g = function
    | IntLit n -> IntLit (abs n)
    | Plus (a, b) -> Plus (g a, g b)
    | Subtract (a, b) -> Subtract (g a, g b)
    | Times (a, b) -> Times (g a, g b)
    | Divide (a, b) -> Divide (g a, g b)

(* 課題2-3 *)
let rec eval = function
    | IntLit n -> n
    | Plus (a, b) -> eval a + eval b
    | Times (a, b) -> eval a * eval b
    | Subtract (a, b) -> eval a - eval b
    | Divide (a, b) ->
        let divider = eval b in
        if divider = 0 then failwith "divided by 0" else eval a / divider
