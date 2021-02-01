let rec zip l1 l2 = match l1, l2 with
    | h1 :: l1, h2 :: l2 -> (h1, h2) :: zip l1 l2
    | [], [] -> []
    | _ -> raise @@ Failure "zip failed"

let rec zip3 l1 l2 l3 = match l1, l2, l3 with
    | h1 :: l1, h2 :: l2, h3 :: l3 -> (h1, h2, h3) :: zip3 l1 l2 l3
    | [], [], [] -> []
    | _ -> raise @@ Failure "zip failed"

let rec unzip = function
    | [] -> [], []
    | (a, b) :: remain ->
        let a', b' = unzip remain in
        a :: a', b :: b'

let rec unzip3 = function
    | [] -> [], [], []
    | (a, b, c) :: remain ->
        let a', b', c' = unzip3 remain in
        a :: a', b :: b', c :: c'

let rec drop n l = match n, l with
    | 0, l -> l
    | n, h :: l -> drop (n-1) l
    | _ -> raise @@ Invalid_argument "cannot drop"
