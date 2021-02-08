let rec fib n = match n with 0 -> 1 | 1 -> 1 | n -> fib (n - 1) + fib (n - 2)
