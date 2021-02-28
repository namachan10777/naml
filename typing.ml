type ty =
    | Int
    | Bool
    | Str
    | Fun of ty * ty
    | Tuple of ty list
    | Poly of int
    | TyVar of int
    | Variant of ty list * Id.t
[@@deriving show]

and ty_var_t = Just of ty * int list | Unknown of int * int list [@@deriving show]

exception UnifyError

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

let store = ref @@ Array.init 2 (fun i -> Unknown (0, [i]))

let count = ref 0

let init () =
    store := Array.init 2 (fun i -> Unknown (0, [i]));
    count := 0

let fresh level =
    let arr_len = Array.length !store in
    if !count >= arr_len then (
      store :=
        Array.concat
          [!store; Array.init arr_len (fun i -> Unknown (level, [i + arr_len]))] ;
      count := 1 + !count ;
      TyVar (!count - 1) )
    else (
      count := 1 + !count ;
      TyVar (!count - 1))

let rec unify t1 t2 = match t1, t2 with
    | TyVar v1, TyVar v2 -> begin match !store.(v1), !store.(v2) with
        | Unknown (level1, l1), Unknown (level2, l2) ->
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Unknown (min level1 level2, l)) l |> ignore
        | Unknown (level1, l1), Just (ty, l2) ->
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Just (ty, l)) l |> ignore
        | Just (ty, l2), Unknown (level1, l1) ->
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Just (ty, l)) l |> ignore
        | Just (ty1, l1), Just (ty2, l2) ->
            unify ty1 ty2;
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Just (ty1, l)) l |> ignore
    end
    | _, _ -> raise UnifyError
