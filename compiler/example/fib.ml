let rec fib n = if n = 0 || n = 1 then 1 else fib (n - 1) + fib (n - 2)

let x = print_int @@ fib 35
