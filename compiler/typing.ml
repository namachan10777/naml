(* Algorithm W*)

type id_t = Ast.id_t
[@@deriving show]

type pat_t = PVar of string * Types.t
[@@deriving show]

type t =
    | Int of int
    | Bool of bool
    | Var of id_t
    | App of t * t list
    | Let of (pat_t * t) list * t
    | Fun of (string * Types.t) list * t * Types.t
    | Tuple of t list * Types.t list
[@@deriving show]

let rec lookup x = function
    | (y, ty) :: remain -> if x = y then ty else lookup x remain
    | _ -> failwith @@ Printf.sprintf "notfound %s" @@ Ast.show_id_t x


let count = ref 0
let init () =
    count := 99

let fresh level =
    count := !count + 1; Types.Unknown (ref (ref (level, !count, None)))

let rec instantiate level =
    let env = ref [] in
    let lookup i =
        let rec f = function
        | (x, y) :: remain -> if i = x then y else f remain
        | [] ->
            let unk = fresh level in
            env := (i, unk) :: !env;
            unk
        in f !env
    in
    let rec f = function
        | Types.Int -> Types.Int
        | Types.Bool -> Types.Bool
        | Types.Fun (args, ret) -> Types.Fun (List.map f args, f ret)
        | Types.Unknown _ as t -> t
        | Types.Poly i ->
            lookup i
        | Types.Tuple ts ->
            Types.Tuple (List.map f ts)
        | _ -> failwith "instantiate unimplemented"
    in f

let rec unify a b =
    Printf.printf "unify \n  > %s\n  > %s\n" (Types.show a) (Types.show b);
    match a, b with
    | Types.Int, Types.Int -> Types.Int
    | Types.Bool, Types.Bool -> Types.Bool
    | Types.Fun ([], _), Types.Fun ([], _) -> failwith "unreachable"
    | Types.Fun ([], r), b ->
        unify r b
    | a, Types.Fun ([], r) ->
        unify r a
    | Types.Fun ([a1], r1), Types.Fun ([a2], r2) ->
        Types.Fun ([unify a1 a2], unify r1 r2)
    | Types.Fun (a1 :: as1, r1), Types.Fun (a2 :: as2, r2) ->
        begin match unify (Types.Fun (as1, r1)) (Types.Fun (as2, r2)) with
        | Types.Fun (as', r) -> Types.Fun ((unify a1 a2) :: as', r)
        | _ -> failwith "cannot unify fun"
        end
    | Types.Tuple ts1, Types.Tuple ts2 ->
        Types.Tuple (Util.zip ts1 ts2 |> List.map (fun (e1, e2) -> unify e1 e2))
    | Types.Unknown r1, (Types.Unknown r2 as t) -> begin match ! !r1, ! !r2 with
        (* levelが低い方に合わせる（多相性は低い方に推論する）*)
        | (level1, _, None), (level2, _, None) when level1 > level2 ->
            !r1 := ! !r2;
            Printf.printf "unk unk %s(%d) %s(%d)\n" (Types.show a) (1 * Obj.magic !r1) (Types.show b) (1 * Obj.magic !r2);
            t
        | (_, _, None), (_, _, None) ->
            !r2 := ! !r1;
            Printf.printf "unk unk %s(%d) %s(%d)\n" (Types.show a) (1 * Obj.magic !r1) (Types.show b) (1 * Obj.magic !r2);
            t
        | (_, _, Some t ), (_, _, None   ) -> r1 := !r2; t
        | (_, _, None   ), (_, _, Some t ) -> r2 := !r1; t
        | (_, _, Some t1), (_, _, Some t2) -> failwith @@ Printf.sprintf "different unknown type %s %s" (Types.show t1) (Types.show t2)
    end
    | Types.Unknown a, b -> begin match ! !a with
        | (_,   _, Some a) -> unify a b
        | (level, tag, None  ) -> !a := (level, tag, Some b); b
    end
    | a, Types.Unknown b -> begin match ! !b with
        | (_, _  , Some b) -> unify a b
        | (level, tag, None) -> !b := (level, tag, Some a); a
    end
    | a, b -> failwith @@ Printf.sprintf "cannot unify %s, %s" (Types.show a) (Types.show b)

type poly_map_t =
    | Unknown of int
    | Poly of int

let readable global tag =
    let rec f tbl = match tag, tbl with
        | Poly tag, (Poly tag', y) :: remain when tag = tag' ->
            y
        | Unknown tag, (Unknown tag', y) :: remain when tag = tag' ->
            y
        | _, _ :: remain -> f remain
        | (_, []) ->
            global := (tag, List.length !global) :: !global;
            (List.length !global) - 1
    in
        let x = f !global in
        x

let generalize_ty tbl level =
    let rec f = function
    | Types.Int -> Types.Int
    | Types.Bool -> Types.Bool
    | Types.Unknown inner as ty ->
        begin match ! !inner with
        | (_, _, Some(ty)) -> ty
        | (level', tag, None) ->
            if level' > level
            then Types.Poly (readable tbl (Unknown tag))
            else ty
        end
    | Types.Fun (args, ret_ty) ->
        Types.Fun (List.map f args, f ret_ty)
    | Types.Tuple ts ->
        Types.Tuple (List.map f ts)
    | Types.Poly tag ->
        Types.Poly (readable tbl (Poly tag))
    | x -> failwith @@ Printf.sprintf "generalize unimplemented %s" @@ Types.show x
    in f

let rec generalize_pat tbl level =
    let rec f = function
    | PVar (id, ty) -> PVar (id, generalize_ty tbl level ty)
    in f

let generalize tbl level =
    let rec f = 
    function
    | Int i -> Int i
    | Bool b -> Bool b
    | Var id -> Var id
    | App (g, args) -> App (f g, List.map f args)
    | Fun (args, body, ret_ty) ->
        Fun (
            List.map (fun (arg, ty) -> arg, generalize_ty tbl level ty) args,
            f body,
            generalize_ty tbl level ret_ty
        )
    | Tuple (vals, types) ->
        Tuple (List.map f vals, List.map (generalize_ty tbl level) types)
    | Let (defs, expr) ->
        Let (List.map (fun (pat, def) -> (generalize_pat tbl level pat, f def)) defs, f expr)
    in f

type env_t = (id_t * Types.t) list
[@@deriving show]

let rec g env level =
    let (venv, tenv, cenv) = env in
    function
    | Ast.Int i -> Int i, Types.Int
    | Ast.Bool b -> Bool b, Types.Bool
    | Ast.Var id -> Var id, lookup id venv
    | Ast.App (f, args) -> 
        let args, arg_tys = Util.unzip @@ List.map (g env level) args in
        let arg_tys = List.map (instantiate level) arg_tys in
        let f, f_ty = g env level f in
        print_endline "----------------------------------------------------";
        let f_ty' = Types.Fun (arg_tys, fresh level) in
        let f_ty = instantiate level f_ty in
        let f_ty = unify (instantiate level f_ty) f_ty' in
        Printf.printf "=>\n%s\n" @@ Types.show @@ f_ty;
        begin match f_ty with
        | Types.Fun (params, ret_ty) ->
            if (List.length params) > (List.length args)
            then
                let param_tys = Util.drop (List.length arg_tys) params in
                App (f, args), Types.Fun (param_tys, ret_ty)
            else if (List.length params) = (List.length args)
            then
                App (f, args), ret_ty
            else
                failwith "too much argument"
        | _ -> failwith "unmatched app"
        end
    | Ast.Let ([Ast.PVar name, def], expr) ->
        let def, def_ty = g env (level + 1) def in
        let tbl = ref [] in
        let def, def_ty = generalize tbl level def, generalize_ty tbl level def_ty in
        let expr, ty = g (([name], def_ty) :: venv, tenv, cenv) level expr in
        Let ([PVar (name, def_ty), def], expr), ty
    | Ast.Fun (args, body) ->
        let args = List.map (fun arg -> arg, fresh level) args in
        let arg_as_vars = List.map (fun (arg, ty) -> [arg], ty) args in
        let body, body_ty = g (arg_as_vars @ venv, tenv, cenv) level body in
        Fun (args, body, body_ty), Types.Fun (List.map snd args, body_ty)
    | Ast.Tuple tp ->
        let elems, types = Util.unzip @@ List.map (g env level) tp in
        Tuple (elems, types), Types.Tuple types
    | t -> failwith @@ Printf.sprintf "unimplemented: %s" @@ Ast.show t

let f ast =
    fst @@ g (
        Types.pervasive_vals,
        Types.pervasive_types,
        Types.pervasive_ctors
    ) 0 ast
