let rec map f l = match l with
    | x :: xs -> f x :: map f xs
    | [] -> []
