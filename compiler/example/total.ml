let rec total n = if n = 0 then 0 else 1 + total (n-1)

let x = print_int @@ total 5
