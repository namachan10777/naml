type pat_tbl = (string list, Id.t) Tbl.t * (string list, unit) Tbl.t ref

let pervasive_var_env = List.map (fun (id, _) -> (Id.name id, (id, true))) Pervasives.vars
let pervasive_ctor_env = List.map (fun (id, _, _) -> (Id.name id, id)) Pervasives.ctors
let pervasive_type_env = List.map (fun (id, _) -> (Id.name id, id)) Pervasives.types
let pervasive_env = (pervasive_var_env, pervasive_ctor_env, pervasive_type_env)

exception Error of string

let rec duplicate_check = function
    | [] -> false
    | [x] -> false
    | x :: xs -> (not @@ List.for_all (fun x' -> x <> x') xs) || duplicate_check xs

let rec f_ty tenv = function
    | Ast.TInt p -> Ast.TInt p
    | Ast.TBool p -> Ast.TBool p
    | Ast.TString p -> Ast.TString p
    | Ast.TApp (tys, tid, p) ->
        let tid = Tbl.lookup (Id.name tid) tenv |> Tbl.expect (Printf.sprintf "unbound type name %s" (Id.show tid)) in
        Ast.TApp (List.map (f_ty tenv) tys, tid, p)
    | Ast.TVar (id, p) -> Ast.TVar (id, p)
    | Ast.TTuple (tys, p) -> Ast.TTuple (List.map (f_ty tenv) tys, p)
let rec f_tydef tenv = function
    | Ast.Alias ty -> [], Ast.Alias (f_ty tenv ty)
    | Ast.Variant defs ->
        let cids = List.map Util.fst defs in
        let cenv = List.fold_left (fun tbl cid -> Tbl.push (Id.name cid) cid tbl) Tbl.empty cids in
        cenv, Ast.Variant (List.map (fun (cid, p, targs) -> (cid, p, List.map (f_ty tenv) targs)) defs)

let rec f_pat cenv pat =
    let rec collect_free_vars = function
        | Ast.PVar (id, p) -> [id]
        | Ast.PTuple (ps, p) ->
            List.concat @@ List.map collect_free_vars ps
        | Ast.PInt _ -> []
        | Ast.PBool _ -> []
        | Ast.As (ps, p) ->
            List.concat @@ List.map collect_free_vars ps
        | Ast.PCtorApp (_, ps, _) ->
            List.concat @@ List.map collect_free_vars ps
        | Ast.Or (p, ps, _) ->
            let free_vars = collect_free_vars p in
            let other_free_vars = List.map collect_free_vars ps in
            let to_comparable x = x
                |> List.map (List.fold_left (fun acc x -> acc ^ "." ^ x) "")
                |> List.sort compare in
            let picked_sample = to_comparable (List.map Id.name free_vars) in
            if List.for_all (fun free_vars -> picked_sample = to_comparable (List.map Id.name free_vars)) other_free_vars
            then free_vars
            else raise @@ Error "free variables on both side of or patten are not matched"
    in
    let rec replace (cenv, penv) = function
        | Ast.PVar (id, p) -> Ast.PVar (Tbl.lookup (Id.name id) penv |> Tbl.expect "", p)
        | Ast.PBool (b, p) -> Ast.PBool (b, p)
        | Ast.PInt (i, p) -> Ast.PInt (i, p)
        | Ast.PTuple (ps, p) -> Ast.PTuple (List.map (replace (cenv, penv)) ps, p)
        | Ast.As (ps, p) -> Ast.As (List.map (replace (cenv, penv)) ps, p)
        | Ast.Or (p, ps, pos) -> Ast.Or (replace (cenv, penv) p, List.map (replace (cenv, penv)) ps, pos)
        | Ast.PCtorApp (id, args, pos) ->
            let ctor_id = Tbl.lookup (Id.name id) cenv
            |> Tbl.expect (Printf.sprintf "%s unbound identifier %s" (Lex.show_pos_t pos) (Id.show id)) in
            Ast.PCtorApp (ctor_id, List.map (replace (cenv, penv)) args, pos)
    in
    let free_vars = collect_free_vars pat in
    if duplicate_check (List.map Id.name free_vars)
    then raise @@ Error "duplicated variable"
    else free_vars, replace (cenv, List.map (fun id -> (Id.name id, id)) free_vars) pat

let register_names tbl =
    List.fold_left (fun tbl id -> Tbl.push (Id.name id) (id, false) tbl) tbl

let register_enabled_names tbl =
    List.fold_left (fun tbl id -> Tbl.push (Id.name id) (id, true) tbl) tbl

let enable_all tbl =
    Tbl.map (fun (id, _) -> (id, true)) tbl


let rec f env =
    let (venv, cenv, tenv) = env in
    function
    | Ast.Int (i, p) -> Ast.Int (i, p)
    | Ast.Never -> Ast.Never
    | Ast.Bool (b, p) -> Ast.Bool (b, p)
    | Ast.If (cond_e, then_e, else_e, p) ->
        Ast.If (f env cond_e, f env then_e, f env else_e, p)
    | Ast.Tuple (es, p) ->
        Ast.Tuple (List.map (f env) es, p)
    | Ast.Var (id, p) ->
        let id =
            Tbl.lookup (Id.name id) venv
            |> Tbl.expect
                 (Printf.sprintf "%s unbound identifier %s" (Lex.show_pos_t p)
                    (Id.show id))
        in
        if snd id 
        then Ast.Var (fst id, p)
        else raise @@ Error "This kind of expression is not allowed as right-hand side of `let rec`"
    | Ast.Fun (arg, body, p) ->
        Ast.Fun (arg, f (Tbl.push (Id.name arg) (arg, true) @@ enable_all venv, cenv, tenv) body, p)
    | Ast.App (g, arg, p) ->
        Ast.App (f env g, f env arg, p)
    | Ast.CtorApp (cid, p, args) ->
        let cid =
            Tbl.lookup (Id.name cid) cenv
            |> Tbl.expect
                 (Printf.sprintf "%s unbound identifier %s" (Lex.show_pos_t p)
                    (Id.show cid))
        in
        Ast.CtorApp (cid, p, List.map (f env) args)
    | Ast.Match (target, arms) ->
        let target = f env target in
        let arms =
            List.map
            (fun (pat, p, guard, body) ->
                let vars, pat = f_pat cenv pat in
                let env = (register_enabled_names venv vars, cenv, tenv) in
                let guard = f env guard in
                let body = f env body in
                (pat, p, guard, body))
            arms
        in
        Ast.Match (target, arms)
    | Ast.Let (defs, e, p) ->
        let vars, pats =
            defs |> List.map Util.fst
            |> List.map (fun p -> f_pat cenv p)
            |> Util.unzip
        in
        let def_exps = defs |> List.map Util.trd |> List.map (f env) in
        let defs = Util.zip3 pats (List.map Util.snd defs) def_exps in
        let names = List.concat vars in
        if duplicate_check (List.map Id.name names)
        then raise @@ Error "Variable bound several times in this matching"
        else
            let venv = register_enabled_names venv names in
            Ast.Let (defs, f (venv, cenv, tenv) e, p)
    | Ast.LetRec (defs, e, p) ->
        let ids = List.map Util.fst defs in
        if duplicate_check (List.map Id.name ids)
        then raise @@ Error "Variable bound several times in this matching"
        else
            let venv = register_names venv ids in
            let def_exps = defs |> List.map Util.trd |> List.map (f (venv, cenv, tenv)) in
            let defs = Util.zip3 ids (List.map Util.snd defs) def_exps in
            Ast.LetRec (defs, f (enable_all venv, cenv, tenv) e, p)
    | Ast.Type (defs, expr) ->
        let tids = List.map (fun (tid, _, _, _) -> tid) defs in
        let tenv = List.fold_left (fun tbl tid -> Tbl.push (Id.name tid) tid tbl) tenv tids in
        let cids, tys = Util.unzip @@ List.map (fun (_, _, _, tydef) -> f_tydef tenv tydef) defs in
        let targs, ps = Util.unzip @@ List.map (fun (_, targ, p, _) -> targ, p) defs in
        let cenv = (List.concat cids) @ cenv in
        let defs = Util.zip4 tids targs ps tys in
        Ast.Type (defs, f (venv, cenv, tenv) expr)
