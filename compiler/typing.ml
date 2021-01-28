(* Algorithm W*)

type ty_t =
    | Int
    | Str
    | Bool
    | Arrow of ty_t * ty_t
    | List of ty_t
    | Var of string

type pat_t =
    | PEmp of ty_t ref
    | PCons of (pat_t * ty_t ref) * (pat_t * ty_t ref)
    | PInt of int
    | PBool of bool
    | PVar of string * ty_t ref
    | PTuple of pat_t list * ty_t ref
    | As of pat_t list * ty_t ref
    | PCtorApp of string list * pat_t * ty_t ref
    | PCtor of string list * ty_t ref
[@@deriving show]

type t =
    | Emp of ty_t ref
    | Int of int
    | Bool of bool
    | Var of string list * ty_t ref
    | Tuple of t list * ty_t ref
    | If of (t * ty_t ref) * (t * ty_t ref) * (t * ty_t ref)
    | Let of string * (t * ty_t ref) * (t * ty_t ref)
    | LetRec of string * (t * ty_t ref) * (t * ty_t ref)
    | Fun of string list * (t * ty_t ref)
    | Match of (t * ty_t ref) * (pat_t * (t * ty_t ref) * (t * ty_t ref)) list
    | App of (t * ty_t ref) * (t * ty_t ref)
    | ArrayAssign of (t * ty_t ref) * (t * ty_t ref) * (t * ty_t ref)
[@@deriving show]
