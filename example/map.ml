type 'a list_t = Nil | Cons of 'a * 'a list_t

let rec map f l = match l with Nil -> Nil | Cons (x, l) -> Cons (f x, map f l)

let x = map (fun x -> x + 1) (Cons (1, Cons (2, Cons (3, Nil))))
