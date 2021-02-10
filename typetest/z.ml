let z f = (fun x -> f (fun y -> x x y)) (fun x -> f (fun y -> x x y))

let fib = z (fun f n -> if n = 0 || n = 1 then 1 else f (n - 1) + f (n - 2))

let x = print_int @@ fib 6
