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
[@@deriving show]

let rec lookup x = function
    | (y, ty) :: remain -> if x = y then ty else lookup x remain
    | _ -> raise @@ Failure "notfound"


let count = ref 0
let init () =
    count := 99

let fresh level =
    count := !count + 1; Types.Unknown (level, ref (ref (!count, None)))

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
        | _ -> failwith "instantiate unimplemented"
    in f

let rec unify a b = match a, b with
    | Types.Int, Types.Int -> Types.Int
    | Types.Bool, Types.Bool -> Types.Bool
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
    | Types.Unknown (_, r1), (Types.Unknown (_, r2) as t) -> begin match ! !r1, ! !r2 with
        | (_, None), (_, None) -> r1 := !r2; t
        | (_, Some t), (_, None) -> r1 := !r2; t
        | (_, None), (_, Some t) -> r2 := !r1; t
        | _ -> failwith @@ Printf.sprintf "different unknown type %s %s" (Types.show a) (Types.show b)
    end
    | Types.Unknown (level, a), b -> begin match ! !a with
        | (_, Some a) -> unify a b
        | (tag, None) -> !a := (tag, Some b); b
    end
    | a, Types.Unknown (level, b) -> begin match ! !b with
        | (_, Some b) -> unify a b
        | (tag, None) -> !b := (tag, Some a); a
    end
    | a, b -> failwith @@ Printf.sprintf "cannot unify %s, %s" (Types.show a) (Types.show b)

let readable tbl tag =
    let rec f = function
        | (x, y) :: remain ->
            if tag = x
            then y
            else f remain
        | [] ->
            tbl := (tag, List.length !tbl) :: !tbl;
            (List.length !tbl) - 1
    in f !tbl

let generalize_ty level =
    let tbl = ref [] in
    let rec f = function
    | Types.Int -> Types.Int
    | Types.Bool -> Types.Bool
    | Types.Unknown (level', inner) as ty -> begin match ! !inner with
        | (_, Some(ty)) -> ty
        | (tag, None) ->
            if level' > level
            then Types.Poly (readable tbl tag)
            else ty
        end
    | Types.Fun (args, ret_ty) ->
        Types.Fun (List.map f args, f ret_ty)
    | _ -> failwith @@ "generalize unimplemented"
    in f

let rec generalize level = function
    | Int i -> Int i
    | Bool b -> Bool b
    | Var id -> Var id
    | App (f, args) -> App (generalize level f, List.map (generalize level) args)
    | Fun (args, body, ret_ty) ->
        Fun (
            List.map (fun (arg, ty) -> arg, generalize_ty level ty) args,
            generalize level body,
            generalize_ty level ret_ty
        )
    | _ -> failwith "unimplemented generalize"

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
        (*Printf.printf "try unify %s\n" @@ Types.show @@ instantiate level f_ty;*)
        (*Printf.printf "try unify %s\n" @@ Types.show @@ Types.Fun (arg_tys, fresh level);*)
        let f_ty = unify (instantiate level f_ty) @@ Types.Fun (arg_tys, fresh level) in
        begin match f_ty with
        | Types.Fun (_, ret_ty) -> App (f, args), ret_ty
        | _ -> failwith "unmatched app"
        end
    | Ast.Let ([Ast.PVar name, def], expr) ->
        let def, def_ty = g env (level + 1) def in
        let def, def_ty = generalize level def, generalize_ty level def_ty in
        let expr, ty = g (([name], def_ty) :: venv, tenv, cenv) level expr in
        Let ([PVar (name, def_ty), def], expr), ty
    | Ast.Fun (args, body) ->
        let args = List.map (fun arg -> arg, fresh level) args in
        let venv = List.map (fun (arg, ty) -> [arg], ty) args in
        let body, body_ty = g (venv, tenv, cenv) level body in
        Fun (args, body, body_ty), Types.Fun (List.map snd args, body_ty)
    | _ -> failwith "unimplemented"

let f ast =
    fst @@ g (
        Types.pervasive_vals,
        Types.pervasive_types,
        Types.pervasive_ctors
    ) 0 ast
