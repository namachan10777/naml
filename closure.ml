type t =
    | Never
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Div of t * t
    | Mod of t * t
    | Gret of t * t
    | Less of t * t
    | Eq of t * t * Types.t
    | Neq of t * t * Types.t
    | Or of t * t
    | And of t * t
    | Append of t * t * Types.t
    | ArrayAssign of t * t * t * Types.t
    | Assign of t * t * Types.t
    | Ref of t * Types.t
    | Not of t
    | Neg of t
    | ArrayAccess of t * t * Types.t
    | PrintString of t
    | PrintInt of t
    | Failwith of t
    | Int of int
    | Bool of bool
    | Var of Id.t * Types.t
    | CtorApp of Id.t * (t * Types.t) list * Types.t
    | Tuple of (t * Types.t) list
    | If of t * t * t * Types.t
    | Let of (Flatlet.pat_t * t * Types.t) * t * Types.t
    | Match of t * Types.t * ((Flatlet.pat_t * t * t) list) * Types.t
    | App of t * t * Types.t * Types.t
    | Fun of (Id.t * Types.t) list * t * Types.t
[@@deriving show]
and fun_t = (Id.t * Types.t) list * t
[@@deriving show]

let rec shrink = function
    | Flatlet.Add (lhr, rhr) -> Add(shrink lhr, shrink rhr)
    | Flatlet.Sub (lhr, rhr) -> Sub(shrink lhr, shrink rhr)
    | Flatlet.Mul (lhr, rhr) -> Mul(shrink lhr, shrink rhr)
    | Flatlet.Div (lhr, rhr) -> Div(shrink lhr, shrink rhr)
    | Flatlet.Mod (lhr, rhr) -> Mod(shrink lhr, shrink rhr)
    | Flatlet.Eq (lhr, rhr, ty) -> Eq(shrink lhr, shrink rhr, ty)
    | Flatlet.Neq (lhr, rhr, ty) -> Neq(shrink lhr, shrink rhr, ty)
    | Flatlet.Append (lhr, rhr, ty) -> Append(shrink lhr, shrink rhr, ty)
    | Flatlet.ArrayAssign (target, idx, inner, ty) -> ArrayAssign(shrink target, shrink idx, shrink inner, ty)
    | Flatlet.ArrayAccess (lhr, rhr, ty) -> ArrayAccess(shrink lhr, shrink rhr, ty)
    | Flatlet.Assign (lhr, rhr, ty) -> Assign(shrink lhr, shrink rhr, ty)
    | Flatlet.Ref (e, ty) -> Ref(shrink e, ty)
    | Flatlet.Neg e -> Neg(shrink e)
    | Flatlet.Not e -> Not(shrink e)
    | Flatlet.PrintInt e -> PrintInt(shrink e)
    | Flatlet.PrintString e -> PrintString(shrink e)
    | Flatlet.Failwith e -> Failwith(shrink e)
    | Flatlet.Gret (lhr, rhr) -> Gret(shrink lhr, shrink rhr)
    | Flatlet.Less (lhr, rhr) -> Less(shrink lhr, shrink rhr)
    | Flatlet.Or (lhr, rhr) -> Less(shrink lhr, shrink rhr)
    | Flatlet.And (lhr, rhr) -> Less(shrink lhr, shrink rhr)
    | Flatlet.Never -> Never
    | Flatlet.Int i -> Int i
    | Flatlet.Bool b -> Bool b
    | Flatlet.Var (id, ty) -> Var (id, ty)
    | Flatlet.If (cond, then_e, else_e, ty) -> If (shrink cond, shrink then_e, shrink else_e, ty)
    | Flatlet.Let ((pat, def, ty), (expr, expr_ty)) ->
        Let (
            (pat, shrink def, ty),
            shrink expr, expr_ty
        )
    | Flatlet.Tuple es -> Tuple (List.map (fun (e, ty) -> shrink e, ty) es)
    | Flatlet.Match (target, target_ty, arms, ty) ->
        Match (
            shrink target, target_ty,
            (List.map (fun (pat, guard, expr) -> pat, shrink guard, shrink expr) arms),
            ty
        )
    | Flatlet.CtorApp (id, args, ty) ->
        CtorApp (id, List.map (fun (arg, ty) -> shrink arg, ty) args, ty)
    | Flatlet.App ((f, f_ty), (arg, arg_ty)) ->
        begin match f_ty with
        | Types.Fun (arg_ty, ret_ty) -> App (shrink f, shrink arg, arg_ty, ret_ty)
        | _ -> failwith ""
        end
    | Flatlet.Fun _ as f ->
        let rec fold_fun = function
        | Flatlet.Fun ((arg, arg_ty), (Flatlet.Fun _ as f, _)) ->
            let args, body, body_ty = fold_fun f in
            (arg, arg_ty) :: args, body, body_ty
        | Flatlet.Fun ((arg, arg_ty), (body, body_ty)) ->
            [arg, arg_ty], (shrink body), body_ty
        | _ -> failwith ""
        in
        let args, body, body_ty = fold_fun f in
        let args = List.map (fun (id, ty) -> id, ty) args in
        Fun (args, body, body_ty)

let sub lhr rhr =
    List.filter (fun (l_id, l_ty) -> List.for_all ((<>) l_id) rhr) lhr

let rec collect_free_ids_pat = function
    | Flatlet.PBool _ -> []
    | Flatlet.PInt _ -> []
    | Flatlet.PVar (id, _) -> [id]
    | Flatlet.PTuple ps -> List.concat @@ List.map (fun (p, _) -> collect_free_ids_pat p) ps
    | Flatlet.PAs (ps, _) -> List.concat @@ List.map collect_free_ids_pat ps
    | Flatlet.POr (p, _, _) -> collect_free_ids_pat p
    | Flatlet.PCtorApp (_, ps, _) -> List.concat @@ List.map (fun (p, _) -> collect_free_ids_pat p) ps

let rec collect_free_variable = function
    | Var (id, ty) -> [id, ty]
    | Never -> []
    | Int _ -> []
    | Bool _ -> []
    | CtorApp _ -> []
    | Add (lhr, rhr) -> collect_free_variable lhr @ collect_free_variable rhr
    | Sub (lhr, rhr) -> collect_free_variable lhr @ collect_free_variable rhr
    | Mul (lhr, rhr) -> collect_free_variable lhr @ collect_free_variable rhr
    | Div (lhr, rhr) -> collect_free_variable lhr @ collect_free_variable rhr
    | Mod (lhr, rhr) -> collect_free_variable lhr @ collect_free_variable rhr
    | Eq (lhr, rhr, _) -> collect_free_variable lhr @ collect_free_variable rhr
    | Neq (lhr, rhr, _) -> collect_free_variable lhr @ collect_free_variable rhr
    | ArrayAccess (lhr, rhr, _) -> collect_free_variable lhr @ collect_free_variable rhr
    | ArrayAssign (target, idx, inner, _) -> collect_free_variable target @ collect_free_variable idx @ collect_free_variable inner
    | Assign (lhr, rhr, _) -> collect_free_variable lhr @ collect_free_variable rhr
    | Append (lhr, rhr, _) -> collect_free_variable lhr @ collect_free_variable rhr
    | And (lhr, rhr) -> collect_free_variable lhr @ collect_free_variable rhr
    | Or (lhr, rhr) -> collect_free_variable lhr @ collect_free_variable rhr
    | Gret (lhr, rhr) -> collect_free_variable lhr @ collect_free_variable rhr
    | Less (lhr, rhr) -> collect_free_variable lhr @ collect_free_variable rhr
    | Ref (e, _) -> collect_free_variable e
    | Not e -> collect_free_variable e
    | Neg e -> collect_free_variable e
    | PrintInt e -> collect_free_variable e
    | PrintString e -> collect_free_variable e
    | Failwith e -> collect_free_variable e
    | Tuple es ->
        Util.uniq @@ List.concat @@ List.map (fun (e, _) -> collect_free_variable e) es
    | If (cond, then_e, else_e, _) ->
        Util.uniq @@ (collect_free_variable cond) @ (collect_free_variable then_e) @ (collect_free_variable else_e)
    | Fun (args, body, _) ->
        let binds = List.map fst args in
        Util.uniq @@ sub (collect_free_variable body) binds
    | Let ((pat, def, _), expr, _) ->
        let binds = collect_free_ids_pat pat in
        let def_frees = collect_free_variable def in
        Util.uniq @@ (sub (collect_free_variable expr @ def_frees) binds)
    | Match (target, _, arms, _) ->
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
    | Add (lhr, rhr) -> Add (replace tbl lhr, replace tbl rhr)
    | Sub (lhr, rhr) -> Sub (replace tbl lhr, replace tbl rhr)
    | Mul (lhr, rhr) -> Mul (replace tbl lhr, replace tbl rhr)
    | Div (lhr, rhr) -> Div (replace tbl lhr, replace tbl rhr)
    | Mod (lhr, rhr) -> Mod (replace tbl lhr, replace tbl rhr)
    | Gret (lhr, rhr) -> Gret (replace tbl lhr, replace tbl rhr)
    | Less (lhr, rhr) -> Less (replace tbl lhr, replace tbl rhr)
    | And (lhr, rhr) -> And (replace tbl lhr, replace tbl rhr)
    | Or (lhr, rhr) -> Or (replace tbl lhr, replace tbl rhr)
    | Assign (lhr, rhr, ty) -> Assign (replace tbl lhr, replace tbl rhr, ty)
    | Eq (lhr, rhr, ty) -> Eq (replace tbl lhr, replace tbl rhr, ty)
    | Neq (lhr, rhr, ty) -> Neq (replace tbl lhr, replace tbl rhr, ty)
    | ArrayAssign (target, idx, inner, ty) -> ArrayAssign (replace tbl target, replace tbl idx, replace tbl inner, ty)
    | ArrayAccess (lhr, rhr, ty) -> ArrayAccess (replace tbl lhr, replace tbl rhr, ty)
    | Append (lhr, rhr, ty) -> Append (replace tbl lhr, replace tbl rhr, ty)
    | Ref (e, ty) -> Ref (replace tbl e, ty)
    | Neg e -> Neg (replace tbl e)
    | Not e -> Not (replace tbl e)
    | PrintInt e -> PrintInt (replace tbl e)
    | PrintString e -> PrintString (replace tbl e)
    | Failwith e -> Failwith (replace tbl e)
    | If (cond_e, then_e, else_e, ty) ->
        If (replace tbl cond_e, replace tbl then_e, replace tbl else_e, ty)
    | Tuple es -> Tuple (List.map (fun (e, ty) -> replace tbl e, ty) es)
    | App (f, arg, f_ty, arg_ty) -> App (replace tbl f, replace tbl arg, f_ty, arg_ty)
    | CtorApp (id, args, ty) -> CtorApp (id, List.map (fun (arg, ty) -> replace tbl arg, ty) args, ty)
    | Let ((pat, def, def_ty), expr, ty) ->
        Let ((pat, replace tbl def, def_ty), replace tbl expr, ty)
    | Match (target, target_ty, arms, ty) ->
        let arms = List.map (fun (pat, guard, expr) -> pat, replace tbl guard, replace tbl expr) arms in
        Match (replace tbl target, target_ty, arms, ty)
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
type ids_t = (Id.t * Types.t) list [@@deriving show]

let rec conv mask = function
    | Add (lhr, rhr) -> Add (conv mask lhr, conv mask rhr)
    | Sub (lhr, rhr) -> Sub (conv mask lhr, conv mask rhr)
    | Mul (lhr, rhr) -> Mul (conv mask lhr, conv mask rhr)
    | Div (lhr, rhr) -> Div (conv mask lhr, conv mask rhr)
    | Mod (lhr, rhr) -> Mod (conv mask lhr, conv mask rhr)
    | Less (lhr, rhr) -> Less (conv mask lhr, conv mask rhr)
    | Gret (lhr, rhr) -> Gret (conv mask lhr, conv mask rhr)
    | Or (lhr, rhr) -> Or (conv mask lhr, conv mask rhr)
    | And (lhr, rhr) -> And (conv mask lhr, conv mask rhr)
    | Append (lhr, rhr, ty) -> Append (conv mask lhr, conv mask rhr, ty)
    | Assign (lhr, rhr, ty) -> Assign (conv mask lhr, conv mask rhr, ty)
    | ArrayAssign (target, idx, inner, ty) -> ArrayAssign (conv mask target, conv mask idx, conv mask inner, ty)
    | ArrayAccess (lhr, rhr, ty) -> ArrayAccess (conv mask lhr, conv mask rhr, ty)
    | Eq (lhr, rhr, ty) -> Eq (conv mask lhr, conv mask rhr, ty)
    | Neq (lhr, rhr, ty) -> Neq (conv mask lhr, conv mask rhr, ty)
    | Ref (e, ty) -> Ref (conv mask e, ty)
    | Not e -> Not (conv mask e)
    | Neg e -> Neg (conv mask e)
    | PrintInt e -> PrintInt (conv mask e)
    | PrintString e -> PrintString (conv mask e)
    | Failwith e -> Failwith (conv mask e)
    | Fun (args, body, ret_ty) ->
        (* bodyを処理してから *)
        let body = conv mask body in
        let f = Fun (args, body, ret_ty) in
        (* 自由変数を集める *)
        let free_variables = sub (collect_free_variable f) mask in
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
    | CtorApp (id, args, ty) -> CtorApp (id, List.map (fun (arg, arg_ty) -> conv mask arg, arg_ty) args, ty)
    | Tuple es -> Tuple (List.map (fun (e, ty) -> conv mask e, ty) es)
    | If (cond_e, then_e, else_e, ty) -> If (conv mask cond_e, then_e, else_e, ty)
    | App (f, arg, f_ty, arg_ty) -> App (conv mask f, conv mask arg, f_ty, arg_ty)
    | Let ((pat, def, def_ty), expr, ty) ->
        Let ((pat, conv mask def, def_ty), conv mask expr, ty)
    | Match (target, target_ty, arms, ty) ->
        let arms = List.map (fun (pat, guard, expr) -> pat, conv mask guard, conv mask expr) arms in
        Match (conv mask target, target_ty, arms, ty)

let f mask tree = conv mask @@ shrink @@ tree
let pervasives = List.map (fun (id, _) -> id) Pervasives.vars
let f pervasives lets =
    let ids = List.concat @@ List.map (fun (p, _, _) -> collect_free_ids_pat p) lets in
    List.map (fun (pat, def, ty) -> (pat, f (ids @ pervasives) def, ty)) lets


