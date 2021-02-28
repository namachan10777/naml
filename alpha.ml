type pat_tbl = (string list, Id.t) Tbl.t * (string list, unit) Tbl.t ref

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
    | p -> ([], p)

let register_names tbl =
    List.fold_left (fun tbl id -> Tbl.push (Id.name id) id tbl) tbl

let rec duplicate_check = function
    | [] -> false
    | [x] -> false
    | x :: xs -> (not @@ List.for_all (fun x' -> x <> x') xs) || duplicate_check xs

let rec f env = function
    | Ast.Var (id, p) ->
        let id =
            Tbl.lookup (Id.name id) env
            |> Tbl.expect
                 (Printf.sprintf "%s unbound identifier %s" (Lex.show_pos_t p)
                    (Id.show id))
        in
        Ast.Var (id, p)
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
            let env = register_names env names in
            Ast.Let (defs, f env e, p)
    | x -> x
