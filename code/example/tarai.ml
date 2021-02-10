let rec tarai x y z =
    if x < y || x = y then y
    else tarai (tarai (x - 1) y z) (tarai (y - 1) z x) (tarai (z - 1) x y)
in
tarai 12 6 0
