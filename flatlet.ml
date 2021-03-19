type pat_t =
    | PInt of int
    | PBool of bool
    | PVar of Id.t * Types.t
    | PTuple of (pat_t * Types.t) list
    | PAs of pat_t list * Types.t
    | POr of pat_t * pat_t list * Types.t
    | PCtorApp of Id.t * (pat_t * Types.t) list * Types.t
[@@deriving show]

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
    | Let of (pat_t * t * Types.t) * (t * Types.t)
    | Match of t * Types.t * (pat_t * t * t) list * Types.t
    | Fun of (Id.t * Types.t) * (t * Types.t)
    | App of (t * Types.t) * (t * Types.t)
[@@deriving show]

type def_t = pat_t * t * Types.t

let deref_ty t = snd @@ Typing.dereference t

let rec g_pat = function
    | Typing.PInt (i, _) -> PInt i
    | Typing.PBool (b, _) -> PBool b
    | Typing.PVar (id, ty, _) -> PVar (id, deref_ty ty)
    | Typing.PTuple (ps, _) -> PTuple (List.map (fun (p, ty) -> g_pat p, deref_ty ty) ps)
    | Typing.As (ps, ty, _) -> PAs (List.map g_pat ps, deref_ty ty)
    | Typing.Or (p, ps, ty, _) -> POr (g_pat p, List.map g_pat ps, deref_ty ty)
    | Typing.PCtorApp (id, args, ty, _) -> PCtorApp (id, List.map (fun (p, ty) -> g_pat p, deref_ty ty) args, deref_ty ty)

let add_id = Id.lookup ["+"] @@ List.map fst Pervasives.vars
let sub_id = Id.lookup ["-"] @@ List.map fst Pervasives.vars
let mul_id = Id.lookup ["*"] @@ List.map fst Pervasives.vars
let div_id = Id.lookup ["/"] @@ List.map fst Pervasives.vars
let mod_id = Id.lookup ["mod"] @@ List.map fst Pervasives.vars
let gret_id = Id.lookup [">"] @@ List.map fst Pervasives.vars
let less_id = Id.lookup ["<"] @@ List.map fst Pervasives.vars
let eq_id = Id.lookup ["="] @@ List.map fst Pervasives.vars
let neq_id = Id.lookup ["<>"] @@ List.map fst Pervasives.vars
let or_id = Id.lookup ["||"] @@ List.map fst Pervasives.vars
let and_id = Id.lookup ["&&"] @@ List.map fst Pervasives.vars
let append_id = Id.lookup ["@"] @@ List.map fst Pervasives.vars
let assign_id = Id.lookup [":="] @@ List.map fst Pervasives.vars
let arrayassign_id = Id.lookup ["<-"] @@ List.map fst Pervasives.vars
let arrayaccess_id = Id.lookup ["."] @@ List.map fst Pervasives.vars
let ref_id = Id.lookup ["ref"] @@ List.map fst Pervasives.vars
let not_id = Id.lookup ["not"] @@ List.map fst Pervasives.vars
let neg_id = Id.lookup ["<neg>"] @@ List.map fst Pervasives.vars
let print_int_id = Id.lookup ["print_int"] @@ List.map fst Pervasives.vars
let print_string_id = Id.lookup ["print_string"] @@ List.map fst Pervasives.vars
let failwith_id = Id.lookup ["failwith"] @@ List.map fst Pervasives.vars

let ref_type_id = Id.lookup ["ref"] @@ List.map fst Pervasives.types
let list_type_id = Id.lookup ["list"] @@ List.map fst Pervasives.types
let array_type_id = Id.lookup ["array"] @@ List.map fst Pervasives.types

let rec g = function
    | Typing.Never -> failwith "why?"
    | Typing.Int (i, _) -> Int i
    | Typing.Bool (b, _) -> Bool b
    | Typing.And (lhr, rhr, p) -> And (g lhr, g rhr)
    | Typing.Or (lhr, rhr, p) -> Or (g lhr, g rhr)
    | Typing.Var (id, ty, _) -> Var (id, deref_ty ty)
    | Typing.CtorApp (id, _, args, ty) -> CtorApp (id, List.map (fun (arg, arg_ty) -> g arg, deref_ty arg_ty) args, deref_ty ty)
    | Typing.Tuple (es, _) -> Tuple (List.map (fun (arg, arg_ty) -> g arg, deref_ty arg_ty) es)
    | Typing.If (c, t, e, ty, _) -> If (g c, g t, g e, deref_ty ty)
    | Typing.Fun ((arg, arg_ty), (body, ret_ty), _) -> Fun ((arg, deref_ty arg_ty), (g body, deref_ty ret_ty))
    | Typing.App (((Typing.App ((Typing.Var (id, _, _), _), (lhr, _), _), _), (rhr, _), _)) when id = add_id -> Add (g lhr, g rhr)
    | Typing.App (((Typing.App ((Typing.Var (id, _, _), _), (lhr, _), _), _), (rhr, _), _)) when id = sub_id -> Sub (g lhr, g rhr)
    | Typing.App (((Typing.App ((Typing.Var (id, _, _), _), (lhr, _), _), _), (rhr, _), _)) when id = mul_id -> Mul (g lhr, g rhr)
    | Typing.App (((Typing.App ((Typing.Var (id, _, _), _), (lhr, _), _), _), (rhr, _), _)) when id = div_id -> Div (g lhr, g rhr)
    | Typing.App (((Typing.App ((Typing.Var (id, _, _), _), (lhr, _), _), _), (rhr, _), _)) when id = mod_id -> Mod (g lhr, g rhr)
    | Typing.App (((Typing.App ((Typing.Var (id, _, _), _), (lhr, _), _), _), (rhr, _), _)) when id = gret_id -> Gret (g lhr, g rhr)
    | Typing.App (((Typing.App ((Typing.Var (id, _, _), _), (lhr, _), _), _), (rhr, _), _)) when id = less_id -> Less (g lhr, g rhr)
    | Typing.App (((Typing.App ((Typing.Var (id, _, _), _), (lhr, _), _), _), (rhr, ty), _)) when id = eq_id -> Eq (g lhr, g rhr, deref_ty ty)
    | Typing.App (((Typing.App ((Typing.Var (id, _, _), _), (lhr, _), _), _), (rhr, ty), _)) when id = neq_id -> Neq (g lhr, g rhr, deref_ty ty)
    | Typing.App (((Typing.App ((Typing.Var (id, _, _), _), (lhr, ty), _), _), (rhr, _), _)) when id = append_id -> Append (g lhr, g rhr, deref_ty ty)
    | Typing.App (((Typing.App ((Typing.Var (id, _, _), _), (lhr, _), _), _), (rhr, ty), _)) when id = assign_id -> Assign (g lhr, g rhr, deref_ty ty)
    (* TODO *)
    | Typing.App (((Typing.App (((Typing.App ((Typing.Var (id, _, _), _), (target, _), _), _), (idx, _), _)), _), (inner, ty), _))
        when id = arrayassign_id -> ArrayAssign (g target, g idx, g inner, deref_ty ty)
    | Typing.App (((Typing.App ((Typing.Var (id, _, _), _), (lhr, Typing.TVariant ([ty], _)), _), _), (rhr, _), _))
        when id = arrayaccess_id -> ArrayAccess (g lhr, g rhr, deref_ty ty)
    | Typing.App ((Typing.Var (id, _, _), _), (e, ty), _) when id = ref_id -> Ref (g e, deref_ty ty)
    | Typing.App ((Typing.Var (id, _, _), _), (e, _), _) when id = not_id -> Not (g e)
    | Typing.App ((Typing.Var (id, _, _), _), (e, _), _) when id = neg_id -> Neg (g e)
    | Typing.App ((Typing.Var (id, _, _), _), (e, _), _) when id = print_int_id -> PrintInt (g e)
    | Typing.App ((Typing.Var (id, _, _), _), (e, _), _) when id = print_string_id -> PrintString (g e)
    | Typing.App ((Typing.Var (id, _, _), _), (e, _), _) when id = failwith_id -> Failwith (g e)
    | Typing.App ((f, f_ty), (arg, arg_ty), _) -> App ((g f, deref_ty f_ty), (g arg, deref_ty arg_ty))
    | Typing.Let (defs, (expr, ty)) ->
        let ty = deref_ty ty in
        List.fold_right (fun ((pat, _), _, (def, def_ty)) expr -> Let ((g_pat pat, g def, deref_ty def_ty), (expr, ty))) defs (g expr)
    | Typing.LetRec (defs, (expr, ty)) ->
        let ty = deref_ty ty in
        List.fold_right (fun (id, _, (def, def_ty)) expr -> Let ((PVar (id, deref_ty def_ty), g def, deref_ty def_ty), (expr, ty))) defs (g expr)
    | Typing.Match ((target, target_ty), arms, ty) ->
        Match (
            g target, deref_ty target_ty,
            List.map (fun ((pat, _), _, guard, expr) -> (g_pat pat, g guard, g expr)) arms,
            deref_ty ty
        )

let rec f = function
    | Typing.Let (defs, (expr, _)) ->
        List.map (
            fun ((pat, _), _, (def, ty)) ->
                (g_pat pat, g def, snd @@ Typing.dereference ty)
        ) defs @ f expr
    | Typing.LetRec (defs, (expr, _)) ->
        List.map (
            fun (id, _, (def, ty)) ->
                let ty = snd @@ Typing.dereference ty in
                (PVar (id, ty), g def, ty)
        ) defs @ f expr
    | Typing.Never -> []
    | _ -> failwith "toplevel expression"

let strip_exts s =
    let rec f n =
        if s.[n] = '.'
        then n
        else f (n-1)
    in String.sub s 0 (f (String.length s - 1))

let split_path s =
    String.split_on_char '/' s

let capitalize s =
    String.concat "" [
        String.map Char.uppercase_ascii (String.sub s 0 1);
        String.sub s 1 (String.length s - 1);
    ]

let prefix_of_fname s =
    s |> strip_exts |> split_path |> List.map capitalize

let rec take_n n l = match n, l with
    | 0, l -> []
    | _, [] -> []
    | n, x :: xs -> x :: take_n (n-1) xs

let rec add_mod_prefix mod_name = function
    | PInt i -> PInt i
    | PBool b -> PBool b
    | PVar ((pre, name, id), ty) -> PVar ((mod_name @ pre, name, id), ty)
    | PTuple ps -> PTuple (List.map (fun (pat, ty) -> add_mod_prefix mod_name pat, ty) ps)
    | PAs (ps, ty) -> PAs (List.map (add_mod_prefix mod_name) ps, ty)
    | POr (p, ps, ty) -> POr (add_mod_prefix mod_name p, List.map (add_mod_prefix mod_name) ps, ty)
    | PCtorApp (id, args, ty) -> PCtorApp (id, List.map (fun (p, ty) -> add_mod_prefix mod_name p, ty) args, ty)


type venv_t = (Id.t * (int * Types.t)) list
type cenv_t = (Id.t * (int * Types.t list) * (int * Types.t)) list
type tenv_t = (Id.t * (int * Types.t)) list
type lets_t = (pat_t * t * Types.t) list
[@@deriving show]
type modules_t = (venv_t * cenv_t * tenv_t) * lets_t

let type_i_i = Types.Fun (Types.Int, Types.Int)
let type_i_b = Types.Fun (Types.Int, Types.Bool)
let type_b_b = Types.Fun (Types.Bool, Types.Bool)
let type_i_i_i = Types.Fun (Types.Int, Types.Fun (Types.Int, Types.Int))
let type_b_b_b = Types.Fun (Types.Bool, Types.Fun (Types.Bool, Types.Bool))
let type_i_i_b = Types.Fun (Types.Int, Types.Fun (Types.Int, Types.Bool))

let pervasives: modules_t =
    let env = Pervasives.vars, Pervasives.ctors, Pervasives.types in
    let lets = [
        (let x = Id.from_strlist ["x"] in
        let y = Id.from_strlist ["y"] in
        PVar (add_id, type_i_i_i),
        Fun ((x, Types.Int), (Fun ((y, Types.Int), (Add (Var (x, Types.Int), Var (y, Types.Int)), Types.Int)), type_i_i)),
        type_i_i_i);

        (let x = Id.from_strlist ["x"] in
        let y = Id.from_strlist ["y"] in
        PVar (sub_id, type_i_i_i),
        Fun ((x, Types.Int), (Fun ((y, Types.Int), (Sub (Var (x, Types.Int), Var (y, Types.Int)), Types.Int)), type_i_i)),
        type_i_i_i);

        (let x = Id.from_strlist ["x"] in
        let y = Id.from_strlist ["y"] in
        PVar (mul_id, type_i_i_i),
        Fun ((x, Types.Int), (Fun ((y, Types.Int), (Mul (Var (x, Types.Int), Var (y, Types.Int)), Types.Int)), type_i_i)),
        type_i_i_i);

        (let x = Id.from_strlist ["x"] in
        let y = Id.from_strlist ["y"] in
        PVar (div_id, type_i_i_i),
        Fun ((x, Types.Int), (Fun ((y, Types.Int), (Div (Var (x, Types.Int), Var (y, Types.Int)), Types.Int)), type_i_i)),
        type_i_i_i);

        (let x = Id.from_strlist ["x"] in
        let y = Id.from_strlist ["y"] in
        PVar (mod_id, type_i_i_i),
        Fun ((x, Types.Int), (Fun ((y, Types.Int), (Mod (Var (x, Types.Int), Var (y, Types.Int)), Types.Int)), type_i_i)),
        type_i_i_i);

        (let x = Id.from_strlist ["x"] in
        let y = Id.from_strlist ["y"] in
        PVar (gret_id, type_i_i_b),
        Fun ((x, Types.Int), (Fun ((y, Types.Int), (Gret (Var (x, Types.Int), Var (y, Types.Int)), Types.Bool)), type_i_b)),
        type_i_i_b);

        (let x = Id.from_strlist ["x"] in
        let y = Id.from_strlist ["y"] in
        PVar (less_id, type_i_i_b),
        Fun ((x, Types.Int), (Fun ((y, Types.Int), (Less (Var (x, Types.Int), Var (y, Types.Int)), Types.Bool)), type_i_b)),
        type_i_i_b);

        (let x = Id.from_strlist ["x"] in
        let y = Id.from_strlist ["y"] in
        let ty = Types.Fun (Types.Poly 0, Types.Fun (Types.Poly 0, Types.Bool)) in
        PVar (eq_id, ty),
        Fun ((x, Types.Poly 0), (
            Fun (
                (y, Types.Poly 0),
                (Eq (Var (x, Types.Poly 0), Var (y, Types.Poly 0), Types.Poly 0), Types.Bool)
            ),
            Types.Fun (Types.Poly 0, Types.Bool))),
        ty);

        (let x = Id.from_strlist ["x"] in
        let y = Id.from_strlist ["y"] in
        let ty = Types.Fun (Types.Poly 0, Types.Fun (Types.Poly 0, Types.Bool)) in
        PVar (neq_id, ty),
        Fun ((x, Types.Poly 0), (
            Fun (
                (y, Types.Poly 0),
                (Neq (Var (x, Types.Poly 0), Var (y, Types.Poly 0), Types.Poly 0), Types.Bool)
            ),
            Types.Fun (Types.Poly 0, Types.Bool))),
        ty);

        (let x = Id.from_strlist ["x"] in
        let y = Id.from_strlist ["y"] in
        PVar (gret_id, type_b_b_b),
        Fun ((x, Types.Bool), (Fun ((y, Types.Bool), (Or (Var (x, Types.Bool), Var (y, Types.Bool)), Types.Bool)), type_b_b)),
        type_b_b_b);

        (let x = Id.from_strlist ["x"] in
        let y = Id.from_strlist ["y"] in
        PVar (less_id, type_b_b_b),
        Fun ((x, Types.Bool), (Fun ((y, Types.Bool), (And (Var (x, Types.Bool), Var (y, Types.Bool)), Types.Bool)), type_b_b)),
        type_b_b_b);

        (let x = Id.from_strlist ["x"] in
        let y = Id.from_strlist ["y"] in
        let ref_ty = Types.Variant ([Types.Poly 0], ref_id) in
        let ty = Types.Fun (ref_ty, Types.Fun (Types.Poly 0, Types.Tuple [])) in
        PVar (assign_id, ty),
        Fun ((x, ref_ty), (
            Fun (
                (y, Types.Poly 0),
                (Assign (Var (x, ref_ty), Var (y, Types.Poly 0), Types.Poly 0), Types.Tuple [])
            ),
            Types.Fun (Types.Poly 0, Types.Tuple []))),
        ty);

        (let x = Id.from_strlist ["x"] in
        let y = Id.from_strlist ["y"] in
        let list_ty = Types.Variant ([Types.Poly 0], list_type_id) in
        let ty = Types.Fun (list_ty, Types.Fun (Types.Poly 0, list_ty)) in
        PVar (append_id, ty),
        Fun ((x, list_ty), (
            Fun (
                (y, Types.Poly 0),
                (Append (Var (x, list_ty), Var (y, Types.Poly 0), Types.Poly 0), list_ty)
            ),
            Types.Fun (Types.Poly 0, list_ty))),
        ty);

        (let x = Id.from_strlist ["x"] in
        let ref_ty = Types.Variant ([Types.Poly 0], ref_type_id) in
        let ty = Types.Fun (Types.Poly 0, ref_ty) in
        PVar (ref_id, ty),
        Fun ((x, Poly 0), (Ref (Var (x, Types.Poly 0), Types.Poly 0), ref_ty)),
        ty);

        (let x = Id.from_strlist ["x"] in
        PVar (not_id, Types.Fun (Types.Bool, Types.Bool)),
        Fun ((x, Types.Bool), (Not (Var (x, Types.Bool)), Types.Bool)),
        Types.Fun (Types.Bool, Types.Bool));

        (let x = Id.from_strlist ["x"] in
        PVar (neg_id, Types.Fun (Types.Int, Types.Int)),
        Fun ((x, Types.Int), (Neg (Var (x, Types.Int)), Types.Int)),
        Types.Fun (Types.Int, Types.Int));

        (let x = Id.from_strlist ["x"] in
        PVar (print_int_id, Types.Fun (Types.Int, Types.Tuple [])),
        Fun ((x, Types.Int), (PrintInt (Var (x, Types.Int)), Types.Tuple [])),
        Types.Fun (Types.Int, Types.Tuple []));

        (let x = Id.from_strlist ["x"] in
        PVar (print_string_id, Types.Fun (Types.Str, Types.Tuple [])),
        Fun ((x, Types.Str), (PrintString (Var (x, Types.Str)), Types.Tuple [])),
        Types.Fun (Types.Str, Types.Tuple []));

        (let x = Id.from_strlist ["x"] in
        PVar (failwith_id, Types.Fun (Types.Str, Types.Poly 0)),
        Fun ((x, Types.Str), (Failwith (Var (x, Types.Str)), Types.Poly 0)),
        Types.Fun (Types.Str, Types.Poly 0));
    ] in
    (env, lets)

(*
let failwith_id = Id.lookup ["failwith"] @@ List.map fst Pervasives.vars
*)

let (typing_module: modules_t -> string -> string -> modules_t) = fun typed fname src ->
    let (venv, cenv, tenv), lets = typed in
    let ast = Ast.f fname src in
    let alpha = Alpha.f (
        List.map (fun (id, _) -> (Id.name id, (id, true))) venv,
        List.map (fun (id, _, _) -> (Id.name id, id)) cenv,
        List.map (fun (id, _) -> (Id.name id, id)) tenv
    ) ast in
    let typed = snd @@ Typing.f (
        List.map (fun (id, (_, ty)) -> id, Typing.types2typing ty) venv,
        List.map (fun (id, (_, targs), (_, ty)) -> id, (List.map Typing.types2typing targs, Typing.types2typing ty)) cenv,
        tenv
    ) alpha in
    let lets' = f typed in
    let (venv', cenv', tenv') = !Typing.env_ref in
    let venv' = take_n (List.length venv' - List.length venv) venv' in
    let tenv' = take_n (List.length tenv' - List.length tenv) tenv' in
    let cenv' = take_n (List.length cenv' - List.length cenv) cenv' in
    let mod_name = prefix_of_fname fname in
    let venv' = List.map (fun ((pre, name, id), ty) -> (mod_name @ pre, name, id), Typing.dereference ty) venv' in
    let tenv' = List.map (fun ((pre, name, id), ty) -> (mod_name @ pre, name, id), ty) tenv' in
    let cenv' = List.map (fun ((pre, name, id), (args, ty)) ->
        match Typing.dereference (Typing.TTuple args) with
        | targs_polyness, Types.Tuple targs -> (mod_name @ pre, name, id), (targs_polyness, targs), (Typing.dereference ty)
        | _ -> failwith ""
    ) cenv' in
    let lets' = List.map (fun (pat, def, def_ty) -> add_mod_prefix mod_name pat, def, def_ty) lets' in
    let env = (venv' @ venv, cenv @ cenv', tenv' @ tenv) in
    (env, lets' @ lets)
