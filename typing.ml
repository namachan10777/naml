type ty =
    | Int
    | Bool
    | Str
    | Fun of ty * ty
    | Tuple of ty list
    | Poly of int
    | Var of ty_var_t
    | Variant of ty list * Id.t
[@@deriving show]

and ty_var_t = Just of ty * int list | Unknown of int list [@@deriving show]

type pat_t =
    | PInt of int * Lex.pos_t
    | PBool of bool * Lex.pos_t
    | PVar of Id.t * ty * Lex.pos_t
    | PTuple of (pat_t * ty) list * Lex.pos_t
    | As of pat_t list * ty * Lex.pos_t
    | Or of pat_t * pat_t list * ty * Lex.pos_t
    | PCtorApp of Id.t * (pat_t * ty) list * ty * Lex.pos_t
    | PCtor of Id.t * ty * Lex.pos_t

type tydef_t =
    | Variant of (Id.t * Lex.pos_t * Types.t list) list
    | Alias of Types.t

type t =
    | Never
    | Int of int * Lex.pos_t
    | Bool of bool * Lex.pos_t
    | Var of Id.t * ty * Lex.pos_t
    | CtorApp of Id.t * Lex.pos_t * (t * ty) list * ty
    | Tuple of (t * ty) list * Lex.pos_t
    | If of t * t * t * ty * Lex.pos_t
    | Let of (pat_t * Lex.pos_t * (t * ty)) list * (t * ty) * bool
    | LetRec of (Id.t * Lex.pos_t * (t * ty)) list * (t * ty) * bool
    | Fun of (Id.t * ty) * (t * ty) * Lex.pos_t
    | Match of (t * ty) * ((pat_t * ty) * Lex.pos_t * t * (t * ty)) list
    | App of (t * ty) * (t * ty) * Lex.pos_t
    | Type of (Id.t * Lex.pos_t * (string * Lex.pos_t) list * tydef_t) list * t

let store = ref @@ Array.init 2 (fun i -> Unknown [i])

let count = ref 0

let fresh () =
    let arr_len = Array.length !store in
    if !count >= arr_len then (
      store :=
        Array.concat
          [!store; Array.init arr_len (fun i -> Unknown [i + arr_len])] ;
      let ret = !store.(!count) in
      count := 1 + !count ;
      ret )
    else
      let ret = !store.(!count) in
      count := 1 + !count ;
      ret
