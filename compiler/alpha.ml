type id_t = string list * int 
[@@deriving show]

type pat_t =
    | PEmp
    | PCons of pat_t * pat_t
    | PInt of int
    | PBool of bool
    | PVar of string
    | PTuple of pat_t list
    | As of pat_t list
    | PCtorApp of string list * pat_t
    | PCtor of string list
[@@deriving show]

type t =
    | Emp
    | Int of int
    | Bool of bool
    | Var of string list
    | Tuple of t list
    | If of t * t * t
    | Let of string * t * t
    | LetRec of string * t * t
    | Fun of string list * t
    | Match of t * (pat_t * t * t) list
    | App of t * t
    | ArrayAssign of t * t * t
[@@deriving show]

type ty_t =
    | TId of string list
    | TVar of id_t
    | TTuple of ty_t list
    | TApp of ty_t * id_t
[@@deriving show]

type tydef_t =
    | Variant of (id_t * ty_t) list
    | Alias of ty_t
[@@deriving show]

type stmt_t =
    | LetS of (pat_t * t) list
    | LetRecS of (pat_t * t) list
    | Type of (string * string list * tydef_t) list
[@@deriving show]

let count = ref 0
let fresh () =
    count := 1 + !count;
    string_of_int !count
