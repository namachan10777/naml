let gcd n m =
    let rec f n m =
        if n = 0
        then m
        else f (m mod n) n
    in
    f (min (abs n) (abs m)) (max (abs n) (abs m))

let rec fib = function
    | 1 -> 1
    | 2 -> 1
    | n when n > 0 -> (fib (n-1)) + (fib (n-2))
    | _ -> 0

(* memoize *)
let fib2 n =
    let rec f (acc1, acc2) m =
        if n <= m
        then acc2
        else f (acc2, acc1 + acc2) (m+1)
    in f (0, 1) 1


let prime n =
    (*naive implementation*)
    let rec f primes m =
        if n == List.length primes
        then List.hd primes
        else if List.for_all (fun prime -> m mod prime != 0) primes
        then f (m :: primes) (m + 1)
        else f primes (m + 1)
    in f [2] 3

let rec qsort = function
    | [] -> []
    | [n] -> [n]
    | pivot :: remain ->
        let l1 = List.filter (fun n -> pivot < n) remain in
        let l2 = List.filter (fun n -> pivot >= n) remain in
        List.append (qsort l2) (pivot :: (qsort l1))
