let rec zip l1 l2 = match l1, l2 with
    | h1 :: l1, h2 :: l2 -> (h1, h2) :: zip l1 l2
    | [], [] -> []
    | _ -> raise @@ Failure "zip failed"

let rec zip3 l1 l2 l3 = match l1, l2, l3 with
    | h1 :: l1, h2 :: l2, h3 :: l3 -> (h1, h2, h3) :: zip3 l1 l2 l3
    | [], [], [] -> []
    | _ -> raise @@ Failure "zip failed"
