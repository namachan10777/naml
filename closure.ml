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
    | Let of (pat_t * t * Types.t) list * t * Types.t
    | LetRec of (Id.t * t * Types.t) list * t * Types.t
    | Match of (t * Types.t) * ((pat_t * t * t) list) * Types.t
    | App of t * t * Types.t * Types.t
    | Fun of (Id.t * Types.t) list * t * Types.t
and fun_t = (Id.t * Types.t) list * t

let deref_ty ty = snd @@ Typing.dereference ty

let rec f_pat = function
    | Typing.PInt (i, _) -> PInt i
    | Typing.PBool (b, _) -> PBool b
    | Typing.PVar (id, ty, _) -> PVar (id, deref_ty ty)
    | Typing.As (ps, ty, _) -> As (List.map f_pat ps, deref_ty ty)
    | Typing.Or (p, ps, ty, _) -> Or (f_pat p, List.map f_pat ps, deref_ty ty)
    | Typing.PTuple (ps, _) -> PTuple (List.map (fun (p, ty) -> f_pat p, deref_ty ty) ps)
    | Typing.PCtorApp (id, ps, ty, _) -> PCtorApp (id, List.map (fun (p, ty) -> f_pat p, deref_ty ty) ps, deref_ty ty)

let rec shrink = function
    | Typing.Never -> Never
    | Typing.Int (i, _) -> Int i
    | Typing.Bool (b, _) -> Bool b
    | Typing.Var (id, ty, _) -> Var (id, deref_ty ty)
    | Typing.If (cond, then_e, else_e, ty, _) -> If (shrink cond, shrink then_e, shrink else_e, deref_ty ty)
    | Typing.Let (defs, (expr, expr_ty)) ->
        Let (
            List.map (fun ((pat, pat_ty), _, (def, def_ty)) -> f_pat pat, shrink def, deref_ty def_ty) defs,
            shrink expr, deref_ty expr_ty
        )
    | Typing.LetRec (defs, (expr, expr_ty)) ->
        LetRec (
            List.map (fun (id, _, (def, def_ty)) -> id, shrink def, deref_ty def_ty) defs,
            shrink expr, deref_ty expr_ty
        )
    | Typing.Tuple (es, p) -> Tuple (List.map (fun (e, ty) -> shrink e, deref_ty ty) es)
    | Typing.Match ((target, target_ty), arms, ty) ->
        Match (
            (shrink target, deref_ty target_ty),
            (List.map (fun ((pat, _), _, guard, expr) -> f_pat pat, shrink guard, shrink expr) arms),
            deref_ty ty
        )
    | Typing.CtorApp (id, _, args, ty) ->
        CtorApp (id, List.map (fun (arg, ty) -> shrink arg, deref_ty ty) args, deref_ty ty)
    | Typing.App ((f, f_ty), (arg, arg_ty), _) ->
        begin match deref_ty f_ty with
        | Types.Fun (arg_ty, ret_ty) -> App (shrink f, shrink arg, arg_ty, ret_ty)
        | _ -> failwith ""
        end
    | Typing.Fun _ as f ->
        let rec fold_fun = function
        | Typing.Fun ((arg, arg_ty), (Typing.Fun _ as f, _), _) ->
            let args, body, body_ty = fold_fun f in
            (arg, arg_ty) :: args, body, body_ty
        | Typing.Fun ((arg, arg_ty), (body, body_ty), _) ->
            [arg, arg_ty], (shrink body), body_ty
        | _ -> failwith ""
        in
        let args, body, body_ty = fold_fun f in
        let args = List.map (fun (id, ty) -> id, deref_ty ty) args in
        Fun (args, body, deref_ty body_ty)

let f = shrink
