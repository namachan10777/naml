type pat_t =
    | PInt of int
    | PBool of bool
    | PVar of Id.t * Types.t
    | PTuple of (pat_t * Types.t) list
    | As of pat_t list * Types.t
    | Or of pat_t * pat_t list * Types.t
    | PCtorApp of Id.t * (pat_t * Types.t) list * Types.t

type t =
    | Never
    | Int of int
    | Bool of bool
    | Var of Id.t * Types.t
    | CtorApp of Id.t * (t * Types.t) list * Types.t
    | Tuple of (t * Types.t) list
    | If of t * t * t * Types.t
    | Let of ((pat_t * Types.t) * (t * Types.t)) list * t
    | LetRec of (Id.t * t * Types.t) list * t
    | Fun of (Id.t * Types.t) list * t * Types.t
    | Closure of (Id.t * Types.t) list * t * Types.t
    | Match of (t * Types.t) * ((pat_t * Types.t) * t) * Types.t
    | App of (t * Types.t) list * Types.t
