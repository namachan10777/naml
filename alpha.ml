type pat_tbl = (string list, Id.t) Tbl.t * (string list, unit) Tbl.t ref

let pervasive_var_env = List.map (fun (id, _) -> (Id.name id, (id, true))) Pervasives.vars

exception Error of string

let f_pat (or_tbl, def_tbl) = function
    | Ast.PVar (id, p) -> (
        let name = Id.name id in
        match (Tbl.lookup name or_tbl, Tbl.lookup name !def_tbl) with
        | Some id, _ -> ([id], Ast.PVar (id, p))
        | None, None ->
            def_tbl := Tbl.push (Id.name id) () !def_tbl ;
            ([id], Ast.PVar (id, p))
        | None, Some _ ->
            failwith
            @@ Printf.sprintf "%s duplicated id %s" (Lex.show_pos_t p)
                 (Id.show id) )
    | p -> failwith "unimplemented"

let register_names tbl =
    List.fold_left (fun tbl id -> Tbl.push (Id.name id) (id, false) tbl) tbl

let register_enabled_names tbl =
    List.fold_left (fun tbl id -> Tbl.push (Id.name id) (id, true) tbl) tbl

let enable_all tbl =
    Tbl.map (fun (id, _) -> (id, true)) tbl

let rec duplicate_check = function
    | [] -> false
    | [x] -> false
    | x :: xs -> (not @@ List.for_all (fun x' -> x <> x') xs) || duplicate_check xs

let rec f (env: (string list, Id.t * bool) Tbl.t) = function
    | Ast.Int (i, p) -> Ast.Int (i, p)
    | Ast.Never -> Ast.Never
    | Ast.Bool (b, p) -> Ast.Bool (b, p)
    | Ast.If (cond_e, then_e, else_e, p) ->
        Ast.If (f env cond_e, f env then_e, f env else_e, p)
    | Ast.Tuple (es, p) ->
        Ast.Tuple (List.map (f env) es, p)
    | Ast.Var (id, p) ->
        let id =
            Tbl.lookup (Id.name id) env
            |> Tbl.expect
                 (Printf.sprintf "%s unbound identifier %s" (Lex.show_pos_t p)
                    (Id.show id))
        in
        if snd id 
        then Ast.Var (fst id, p)
        else raise @@ Error "This kind of expression is not allowed as right-hand side of `let rec`"
    | Ast.Fun (arg, body, p) ->
        Ast.Fun (arg, f (Tbl.push (Id.name arg) (arg, true) @@ enable_all env) body, p)
    | Ast.App (g, arg, p) ->
        Ast.App (f env g, f env arg, p)
    | Ast.Let (defs, e, p) ->
        let vars, pats =
            defs |> List.map Util.fst
            |> List.map (fun p -> f_pat (Tbl.empty, ref Tbl.empty) p)
            |> Util.unzip
        in
        let def_exps = defs |> List.map Util.trd |> List.map (f env) in
        let defs = Util.zip3 pats (List.map Util.snd defs) def_exps in
        let names = List.concat vars in
        if duplicate_check (List.map Id.name names)
        then raise @@ Error "Variable bound several times in this matching"
        else
            let env = register_enabled_names env names in
            Ast.Let (defs, f env e, p)
    | Ast.LetRec (defs, e, p) ->
        let ids = List.map Util.fst defs in
        if duplicate_check (List.map Id.name ids)
        then raise @@ Error "Variable bound several times in this matching"
        else
            let env = register_names env ids in
            let def_exps = defs |> List.map Util.trd |> List.map (f env) in
            let defs = Util.zip3 ids (List.map Util.snd defs) def_exps in
            Ast.LetRec (defs, f (enable_all env) e, p)
    | x -> failwith "unimplemented"
