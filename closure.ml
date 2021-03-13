type pat_t =
    | PInt of int
    | PBool of bool
    | PVar of Id.t * Types.t
    | PTuple of (pat_t * Types.t) list
    | As of pat_t list * Types.t
    | Or of pat_t * pat_t list * Types.t
    | PCtorApp of Id.t * (pat_t * Types.t) list * Types.t
[@@deriving show]

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
[@@deriving show]
and fun_t = (Id.t * Types.t) list * t
[@@deriving show]

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

let sub lhr rhr =
    List.filter (fun (l_id, l_ty) -> List.for_all ((<>) l_id) rhr) lhr

let rec collect_free_ids_pat = function
    | PBool _ -> []
    | PInt _ -> []
    | PVar (id, _) -> [id]
    | PTuple ps -> List.concat @@ List.map (fun (p, _) -> collect_free_ids_pat p) ps
    | As (ps, _) -> List.concat @@ List.map collect_free_ids_pat ps
    | Or (p, _, _) -> collect_free_ids_pat p
    | PCtorApp (_, ps, _) -> List.concat @@ List.map (fun (p, _) -> collect_free_ids_pat p) ps

let rec collect_free_variable = function
    | Var (id, ty) -> [id, ty]
    | Never -> []
    | Int _ -> []
    | Bool _ -> []
    | CtorApp _ -> []
    | Tuple es ->
        Util.uniq @@ List.concat @@ List.map (fun (e, _) -> collect_free_variable e) es
    | If (cond, then_e, else_e, _) ->
        Util.uniq @@ (collect_free_variable cond) @ (collect_free_variable then_e) @ (collect_free_variable else_e)
    | Fun (args, body, _) ->
        let binds = List.map fst args in
        Util.uniq @@ sub (collect_free_variable body) binds
    | Let (defs, expr, _) ->
        let binds = List.concat @@ List.map collect_free_ids_pat @@ List.map Util.fst defs in
        let def_frees = List.concat @@ List.map collect_free_variable @@ List.map Util.snd defs in
        Util.uniq @@ (sub (collect_free_variable expr) binds) @ def_frees
    | LetRec (defs, expr, _) ->
        (* recなので定義側の式からも左辺で定義されたシンボルを引く *)
        let binds = List.map Util.fst defs in 
        let def_frees = List.concat @@ List.map collect_free_variable @@ List.map Util.snd defs in
        sub (Util.uniq @@ (collect_free_variable expr) @ def_frees) binds
    | Match ((target, _), arms, _) ->
        let arm_frees =
            List.concat @@ List.map
                (fun (pat, guard, expr) ->
                    sub (collect_free_variable guard @ collect_free_variable expr) (collect_free_ids_pat pat)) arms in
        Util.uniq (collect_free_variable target @ arm_frees)
    | App (f, arg, _, _) -> Util.uniq @@ collect_free_variable f @ collect_free_variable arg

let rec replace tbl = function
    | Never -> Never
    | Int i -> Int i
    | Bool b -> Bool b
    | If (cond_e, then_e, else_e, ty) ->
        If (replace tbl cond_e, replace tbl then_e, replace tbl else_e, ty)
    | Tuple es -> Tuple (List.map (fun (e, ty) -> replace tbl e, ty) es)
    | App (f, arg, f_ty, arg_ty) -> App (replace tbl f, replace tbl arg, f_ty, arg_ty)
    | CtorApp (id, args, ty) -> CtorApp (id, List.map (fun (arg, ty) -> replace tbl arg, ty) args, ty)
    | Let (defs, expr, ty) ->
        Let (List.map (fun (pat, def_e, ty) -> pat, replace tbl def_e, ty) defs, replace tbl expr, ty)
    | LetRec (defs, expr, ty) ->
        LetRec (List.map (fun (id, def_e, ty) -> id, replace tbl def_e, ty) defs, replace tbl expr, ty)
    | Match ((target, target_ty), arms, ty) ->
        let arms = List.map (fun (pat, guard, expr) -> pat, replace tbl guard, replace tbl expr) arms in
        Match ((replace tbl target, target_ty), arms, ty)
    | Fun (args, body, ty) -> Fun (args, replace tbl body, ty)
    | Var (id, ty) -> begin match Tbl.lookup id tbl with
        | Some id -> Var (id, ty)
        | None -> Var (id, ty)
    end

type tys_t = Types.t list
[@@deriving show]
type args_t = (Id.t * Types.t) list
[@@deriving show]

let drop_last l = List.rev @@ List.tl @@ List.rev l

let rec conv = function
    | Fun (args, body, ret_ty) ->
        (* bodyを処理してから *)
        let f = Fun (args, conv body, ret_ty) in
        (* 自由変数を集める *)
        let free_variables = collect_free_variable f in
        (* 自由変数を明示的に渡される引数に変換*)
        let replace_table = List.map (fun (id, ty) -> id, Id.from_strlist ["<cap>"], ty) free_variables in
        let body = replace (List.map (fun (id, id', _) -> id, id') replace_table) body in
        let implicit_args = List.map (fun (_, id, ty) -> id, ty) replace_table in
        let f = Fun (implicit_args @ args, body, ret_ty) in
        let funty = List.fold_right (fun arg_ty ret_ty -> Types.Fun (arg_ty, ret_ty)) (List.map snd args) ret_ty in
        let funtys =
            List.tl @@ List.fold_left
                (fun funtys arg_ty -> (Types.Fun (arg_ty, List.hd funtys) :: funtys)) [funty] @@ List.rev @@ List.map snd implicit_args in
        List.fold_left (fun inner ((arg, arg_ty), funty) -> App (inner, Var (arg, arg_ty), arg_ty, funty)) f (Util.zip free_variables funtys)
    (* replaceで置き換えられているはずなのでそのまま *)
    | Var (id, ty) -> Var (id, ty)
    | Int i -> Int i
    | Bool b -> Bool b
    | Never -> Never
    | CtorApp (id, args, ty) -> CtorApp (id, List.map (fun (arg, arg_ty) -> conv arg, arg_ty) args, ty)
    | Tuple es -> Tuple (List.map (fun (e, ty) -> conv e, ty) es)
    | If (cond_e, then_e, else_e, ty) -> If (conv cond_e, then_e, else_e, ty)
    | App (f, arg, f_ty, arg_ty) -> App (conv f, conv arg, f_ty, arg_ty)
    | Let (defs, expr, ty) ->
        let defs = List.map (fun (pat, def, ty) -> pat, conv def, ty) defs in
        Let (defs, conv expr, ty)
    | LetRec (defs, expr, ty) ->
        let defs = List.map (fun (id, def, ty) -> id, conv def, ty) defs in
        LetRec (defs, conv expr, ty)
    | Match ((target, target_ty), arms, ty) ->
        let arms = List.map (fun (pat, guard, expr) -> pat, conv guard, conv expr) arms in
        Match ((conv target, target_ty), arms, ty)

let f tree = conv @@ shrink @@ tree
