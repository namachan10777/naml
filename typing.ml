type ty =
    | TInt
    | TBool
    | TStr
    | TFun of ty * ty
    | TTuple of ty list
    | Poly of int
    | TyVar of int
    | TVariant of ty list * Id.t
[@@deriving show]

and ty_var_t = Just of ty * int list | Unknown of int * int * int list [@@deriving show]

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

let store = ref @@ Array.init 2 (fun i -> Unknown (0, i, [i]))
type tenv_t = ty_var_t array [@@deriving show]

let count = ref 0

let init () =
    store := Array.init 2 (fun i -> Unknown (0, i, [i]));
    count := 0

let fresh level =
    let arr_len = Array.length !store in
    if !count >= arr_len then (
      store :=
        Array.concat
          [!store; Array.init arr_len (fun i -> Unknown (0, i+arr_len, [i + arr_len]))] ;
      let idx = !count in
      count := 1 + !count ;
      !store.(idx) <- Unknown(level, idx, [idx]);
      TyVar idx )
    else (
      let idx = !count in
      count := 1 + !count ;
      !store.(idx) <- Unknown(level, idx, [idx]);
      TyVar idx)

let rec unify t1 t2 = match t1, t2 with
    | TyVar v1, TyVar v2 -> begin match !store.(v1), !store.(v2) with
        | Unknown (level1, tag, l1), Unknown (level2, _, l2) when level1 < level2 ->
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Unknown (level1, tag, l)) l |> ignore
        | Unknown (level1, _, l1), Unknown (level2, tag, l2) ->
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Unknown (level2, tag, l)) l |> ignore
        | Unknown (_, _, l1), Just (ty, l2) ->
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Just (ty, l)) l |> ignore
        | Just (ty, l1), Unknown (_, _, l2) ->
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Just (ty, l)) l |> ignore
        | Just (ty1, l1), Just (ty2, l2) ->
            unify ty1 ty2;
            let l = l1 @ l2 in
            List.map (fun i -> !store.(i) <- Just (ty1, l)) l |> ignore
    end
    | TyVar v, ty -> begin match !store.(v) with
        | Unknown (_, _, l) ->
            List.map (fun i -> !store.(i) <- Just (ty, l)) l |> ignore
        | Just (ty', l) -> unify ty ty'
    end
    | ty, TyVar v -> begin match !store.(v) with
        | Unknown (_, _, l) ->
            List.map (fun i -> !store.(i) <- Just (ty, l)) l |> ignore
        | Just (ty', l) -> unify ty ty'
    end
    | TInt, TInt -> ()
    | TBool, TBool -> ()
    | TStr, TStr -> ()
    | Poly _, _ -> failwith "uninstantiate poly type"
    |  _, Poly _ -> failwith "uninstantiate poly type"
    | TTuple ts, TTuple ts' ->
        Util.zip ts ts'
        |> List.map (fun (t1, t2) -> unify t1 t2)
        |> ignore
    | TFun (arg1, ret1), TFun (arg2, ret2) ->
        unify arg1 arg2;
        unify ret1 ret2
    | TVariant (tys1, id1), TVariant (tys2, id2) ->
        if id1 <> id2
        then raise UnifyError
        else
            Util.zip tys1 tys2
            |> List.map (fun (t1, t2) -> unify t1 t2)
            |> ignore
    | _, _ -> raise UnifyError
