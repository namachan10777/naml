type 'a list_t =
    | Nil
    | Cons of 'a * 'a list_t

let rec map f l = match l with
    | Nil -> Nil
    | Cons (x, l) -> Cons (f x, map f l)
