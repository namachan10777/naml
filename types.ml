type t =
    | Int
    | Bool
    | Str
    | Fun of t * t
    | Tuple of t list
    | Poly of int
    | Variant of t list * Id.t

type scheme_t = int * t
