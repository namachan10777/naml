type ty_t =
    | TParen of ty_t
    | TId of string list
    | TVar of string
    | TTuple of ty_t list
    | TApp of ty_t * string list
[@@deriving show]

type tydef_t =
    | Variant of (string * ty_t option) list
    | Alias of ty_t
[@@deriving show]

type pat_t =
    | PInt of int
    | PBool of bool
    | PVar of string
    | PEmp
    | PCons of pat_t * pat_t
    | PTuple of pat_t list
    | PParen of pat_t
    | PCtor of string list
    | PCtorApp of string list * pat_t
    | As of pat_t list
[@@deriving show]

type t =
    | Never
    | Emp
    | Int of int
    | Bool of bool
    | Var of string list
    | Ctor of string list
    | Index of t * t | Assign of t * t | ArrayAssign of t * t * t
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Div of t * t
    | Mod of t * t
    | Neg of t
    | Not of t
    | Ref of t
    | Eq of t * t
    | Neq of t * t
    | Or of t * t
    | And of t * t
    | Gret of t * t
    | Less of t * t
    | Cons of t * t
    | Tuple of t list
    | If of t * t * t
    | Let of (pat_t * t) list * t
    | LetRec of (string list * t) list * t
    | Type of (string * string list * tydef_t) list * t
    | Fun of string list * t
    | Match of t * (pat_t * t * t) list
    | App of t * t
    | Seq of t * t
    | Pipeline of t * t
    | Paren of t
[@@deriving show]

type input_t = Lex.t list
[@@deriving show]

type parsed_t = t * Lex.t list
[@@deriving show]

exception SyntaxError of string

let rec split_with f = function
    | hd :: tl ->
        if f hd
        then Some ([], tl)
        else begin match split_with f tl with
        | Some(first_half, second_half) -> Some(hd :: first_half, second_half)
        | None -> None
        end
    | [] -> None

let dbg ast = print_endline @@ show_parsed_t ast
let dbg_i ast = print_endline @@ show_input_t ast

let count = ref 0

let gen_fresh () =
    count := 1 + !count;
    "<anonymous" ^ (string_of_int !count) ^ ">"

let succ_lets = function
    | Lex.Let :: _ -> true
    | Lex.Fun :: _ -> true
    | Lex.Match :: _ -> true
    | Lex.If :: _ -> true
    | _ -> false

let rec take_params = function
    | Lex.LIdent id :: remain ->
        let (params, remain) = take_params remain in
        (id :: params, remain)
    | remain -> ([], remain)

type param_taken_t = string list * input_t
[@@deriving show]

let rec parse_ty input = parse_ty_tuple input

and parse_ty_tuple input = match parse_tapp input with
    | (lhr, Lex.Mul :: remain) -> begin match parse_ty_tuple remain with
        | (TTuple rhr, remain) -> TTuple (lhr :: rhr), remain
        | (rhr, remain) -> TTuple [lhr; rhr], remain
    end
    | t -> t
and parse_tapp input = match parse_ty_term input with
    | (t, (Lex.LIdent _ :: _ as remain)) ->
        let (higher_types, remain) = take_tids remain in
        (List.fold_left (fun t higher_type -> TApp (t, higher_type)) t higher_types, remain)
    | (t, (Lex.UIdent _ :: _ as remain)) ->
        let (higher_types, remain) = take_tids remain in
        (List.fold_left (fun t higher_type -> TApp (t, higher_type)) t higher_types, remain)
    | t -> t
and take_tids input = match parse_tid input with
    | (tid, (Lex.LIdent _ :: _ as remain)) ->
        let (tids, remain) = take_tids remain in
        tid :: tids, remain
    | (tid, (Lex.UIdent _ :: _ as remain)) ->
        let (tids, remain) = take_tids remain in
        tid :: tids, remain
    | (tid, remain) -> [tid], remain
and parse_ty_term = function
    | Lex.TVar id :: remain -> TVar id, remain
    | Lex.LIdent id :: remain -> TId [id], remain
    | Lex.UIdent _ :: _ as remain ->
        let (tid, remain) = parse_tid remain in TId tid, remain
    | Lex.LP :: inner -> begin match parse_ty inner with
        | (inner, RP :: remain) -> TParen inner, remain
        | _ -> raise @@ SyntaxError "paren is not balanced in type"
    end
    | t -> raise @@ SyntaxError (Printf.sprintf "ty_term %s" @@ show_input_t t)
and parse_tid = function
    | Lex.LIdent id :: remain -> [id], remain
    | Lex.UIdent m_id :: Lex.Dot :: Lex.LIdent id :: remain -> [m_id; id], remain
    | Lex.UIdent m_id :: Lex.Dot :: (Lex.UIdent _ :: _ as remain) ->
        let (child, remain) = parse_tid remain in
        (m_id :: child), remain
    | i -> raise @@ SyntaxError (Printf.sprintf "type id %s" @@ show_input_t i)

let rec parse_ty_variant = function
    | Lex.VBar :: Lex.UIdent name :: Lex.Of :: remain -> begin match parse_ty remain with
        | (t, Lex.VBar :: arms) -> begin match parse_ty_variant arms with
            | (Variant arms, remain) -> (Variant ((name, Some t) :: arms), remain)
            | _ -> raise @@ SyntaxError "variant"
        end
        | (t, remain) -> Variant ([name, Some t]), remain
    end
    | Lex.UIdent name :: Lex.Of :: remain -> begin match parse_ty remain with
        | (t, Lex.VBar :: arms) -> begin match parse_ty_variant arms with
            | (Variant arms, remain) -> (Variant ((name, Some t) :: arms), remain)
            | _ -> raise @@ SyntaxError "variant"
        end
        | (t, remain) -> Variant ([name, Some t]), remain
    end
    | Lex.VBar :: Lex.UIdent name :: Lex.VBar :: arms -> begin match parse_ty_variant arms with
        | (Variant arms, remain) -> (Variant ((name, None) :: arms), remain)
        | _ -> raise @@ SyntaxError "variant"
    end
    | Lex.VBar :: Lex.UIdent name :: remain ->
        Variant ([name, None]), remain
    | Lex.UIdent name :: Lex.VBar :: arms -> begin match parse_ty_variant arms with
        | (Variant arms, remain) -> (Variant ((name, None) :: arms), remain)
        | _ -> raise @@ SyntaxError "variant"
    end
    | Lex.UIdent name :: remain ->
        Variant ([name, None]), remain
    | input ->
        let (ty, remain) = parse_ty input in
        Alias ty, remain

let rec parse_pat input = match parse_pat_tuple input with
    | (pat, Lex.As :: rhr) -> begin match parse_pat_tuple rhr with
        | (PTuple tp, remain) -> (As (pat :: tp), remain)
        | (rhr, remain) -> (As [pat; rhr], remain)
    end
    | p -> p
and parse_pat_tuple input = match parse_pat_cons input with
    | (pat, Lex.Comma :: rhr) -> begin match parse_pat_tuple rhr with
        | (PTuple tp, remain) -> (PTuple (pat :: tp), remain)
        | (rhr, remain) -> (PTuple [pat; rhr], remain)
    end
    | p -> p
and parse_pat_cons input = match parse_pat_term input with
    | (pat, Lex.Cons :: rhr) ->
        let (rhr, remain) = parse_pat_cons rhr in
        (PCons (pat, rhr), remain)
    | p -> p
and parse_pat_term = function
    | Lex.LIdent id :: remain -> (PVar id, remain)
    | Lex.UIdent _ :: _ as remain -> begin match parse_pat_ctor remain with
        | (id, Lex.Cons :: remain) -> (PCtor id, Lex.Cons :: remain)
        | (id, Lex.Comma :: remain) -> (PCtor id, Lex.Comma :: remain)
        | (id, remain) ->
            let (arg, remain) = parse_pat_term remain in
            (PCtorApp (id, arg), remain)
    end
    | Lex.Int i :: remain -> (PInt i, remain)
    | Lex.True :: remain -> (PBool true, remain)
    | Lex.False :: remain -> (PBool false, remain)
    | Lex.LP :: Lex.RP :: remain -> (PTuple [], remain)
    | Lex.LB :: Lex.RB :: remain -> (PEmp, remain)
    | Lex.LB :: remain -> begin match parse_pat_list_elem remain with
            | (inner, Lex.RB :: remain) -> (inner, remain)
            | x -> raise @@ SyntaxError "paren is not balanced in pattern"
        end
    | Lex.LP :: inner -> begin match parse_pat inner with
        | (inner, Lex.RP :: remain) -> (PParen inner, remain)
        | _ -> raise @@ SyntaxError "paren is not balanced in pattern"
    end
    | x -> raise @@ SyntaxError (Printf.sprintf "pattern term %s" @@ show_input_t x)
and parse_pat_ctor = function
    | Lex.UIdent id :: Lex.Dot :: remain ->
        let (last, remain) = parse_pat_ctor remain in
        (id :: last, remain)
    | Lex.UIdent id :: remain -> ([id], remain)
    | _ -> raise @@ SyntaxError "ident"
and parse_pat_list_elem input = match parse_pat input with
    | (lhr, Lex.Semicol :: Lex.RB :: remain) ->
        (PCons (lhr, PEmp), Lex.RB :: remain)
    | (lhr, Lex.Semicol :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_pat rhr in (PCons (lhr, PCons (rhr, PEmp)), remain)
    | (lhr, Lex.Semicol :: rhr) -> begin match parse_pat_list_elem rhr with
        | (PCons _ as rhr, remain) -> (PCons (lhr, rhr), remain)
        | (rhr, remain) -> (PCons (lhr, rhr), remain)
    end
    | (x, remain) -> (PCons(x, PEmp), remain)

let rec parse_params stop input = match parse_pat input with
    | (pat, hd :: remain) when hd = stop -> ([pat], hd :: remain)
    | (pat, remain) ->
        let pats, remain = parse_params stop remain in
        (pat :: pats, remain)

let rec parse_expr = function
    | Lex.Let :: Lex.Rec :: remain ->
        begin match parse_params Lex.Eq remain with
        | ([PVar id], Lex.Eq :: remain) ->
            begin match parse_expr remain with
            | (def, Lex.In :: remain) -> 
                let (expr, remain) = parse_expr remain in
                LetRec ([[id], def], expr), remain
            | (def, Lex.AndDef :: remain) -> begin match parse_letrec_ands remain with
                | (defs, Lex.In :: remain) ->
                    let expr, remain = parse_expr remain in
                    LetRec (([id], def) :: defs, expr), remain
                | _ -> raise @@ SyntaxError "let rec"
            end
            | _ -> raise @@ SyntaxError "let rec"
            end
        | (PVar id :: (_ :: _ as params), Lex.Eq :: remain) ->
            begin match parse_expr remain with
            | (def, Lex.In :: remain) -> 
                let (expr, remain) = parse_expr remain in
                (LetRec ([[id], unfold_fun params def], expr), remain)
            | (def, Lex.AndDef :: remain) -> begin match parse_letrec_ands remain with
                | (defs, Lex.In :: remain) ->
                    let expr, remain = parse_expr remain in
                    LetRec (([id], unfold_fun params def) :: defs, expr), remain
                | _ -> raise @@ SyntaxError "let rec"
            end
            | _ -> raise @@ SyntaxError "let"
            end
        | _ -> raise @@ SyntaxError "letrec"
        end
    | Lex.Let :: remain ->
        begin match parse_params Lex.Eq remain with
        | ([pat], Lex.Eq :: remain) ->
            begin match parse_expr remain with
            | (def, Lex.In :: remain) -> 
                let (expr, remain) = parse_expr remain in
                Let ([pat, def], expr), remain
            | (def, Lex.AndDef :: remain) -> begin match parse_let_ands remain with
                | (defs, Lex.In :: remain) ->
                    let expr, remain = parse_expr remain in
                    Let ((pat, def) :: defs, expr), remain
                | _ -> raise @@ SyntaxError "let rec"
            end
            | _ -> raise @@ SyntaxError "let"
            end
        | (PVar id :: (_ :: _ as params), Lex.Eq :: remain) ->
            begin match parse_expr remain with
            | (def, Lex.In :: remain) -> 
                let (expr, remain) = parse_expr remain in
                (Let ([PVar id, unfold_fun params def], expr), remain)
            | (def, Lex.AndDef :: remain) -> begin match parse_let_ands remain with
                | (defs, Lex.In :: remain) ->
                    let expr, remain = parse_expr remain in
                    Let ((PVar id, unfold_fun params def) :: defs, expr), remain
                | _ -> raise @@ SyntaxError "let rec"
            end
            | _ -> raise @@ SyntaxError "let"
            end
        | _ -> raise @@ SyntaxError "let"
        end
    | Lex.Fun :: remain -> begin match parse_params Lex.Arrow remain with
        | (args, Lex.Arrow :: remain) ->
            let (expr, remain) = parse_expr remain in
            (unfold_fun args expr, remain)
        | x -> raise @@ SyntaxError "fun"
    end
    | Lex.If :: cond -> begin match parse_expr cond with
        | (cond, Lex.Then :: then_e) -> begin match parse_expr then_e with
            | (then_e, Lex.Else :: else_e) ->
                let (else_e, remain) = parse_expr else_e in
                (If (cond, then_e, else_e), remain)
            | _ -> raise @@ SyntaxError "if: else not found"
        end
        | _ -> raise @@ SyntaxError "if: then not found"
    end
    | Lex.Match :: remain -> begin match parse_expr remain with
        | (target, Lex.With :: Lex.VBar :: arms) ->
            let (arms, remain) = parse_arms arms in
            (Match (target, arms), remain)
        | (target, Lex.With :: arms) ->
            let (arms, remain) = parse_arms arms in
            (Match (target, arms), remain)
        | _ -> raise @@ SyntaxError "match"
    end
    | others -> parse_seq others
and unfold_fun args expr = 
    let (body, params) = List.fold_left (fun (inner, params) -> function
        | PVar id -> (inner, id :: params)
        | pat ->
            let tmpname = gen_fresh () in
            (Match (Var [tmpname], [pat, Bool true, inner]), tmpname :: params))
        (expr, [])
        (List.rev args)
    in
    Fun (params, body)
and parse_seq input = match parse_assign input with
    | (lhr, Lex.Semicol :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_assign rhr in (Seq (lhr, rhr), remain)
    | (lhr, Lex.Semicol :: rhr) ->
        let (rhr, remain) = parse_seq rhr in (Seq (lhr, rhr), remain)
    | x -> x
and parse_assign input = match parse_arrayassign input with
    | (lhr, Lex.Assign :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_tuple rhr in (Assign (lhr, rhr), remain)
    | (lhr, Lex.Assign :: rhr) ->
        let (rhr, remain) = parse_tuple rhr in (Assign (lhr, rhr), remain)
    | x -> x
and parse_arrayassign input = match parse_tuple input with
    | (Index (arr, idx), Lex.ArrayAssign :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_tuple rhr in (ArrayAssign (arr, idx, rhr), remain)
    | (Index (arr, idx), Lex.ArrayAssign :: rhr) ->
        let (rhr, remain) = parse_tuple rhr in (ArrayAssign (arr, idx , rhr), remain)
    | x -> x
and parse_tuple input = match parse_pipeline input with
    | (lhr, Lex.Comma :: rhr) when succ_lets rhr ->
        let (rhr, remain) = parse_expr rhr in (Tuple [lhr; rhr], remain)
    | (lhr, Lex.Comma :: rhr) -> begin match parse_tuple rhr with
        | (Tuple rhr, remain) -> (Tuple (lhr :: rhr), remain)
        | (rhr, remain) -> (Tuple [lhr; rhr], remain)
    end
    | x -> x
and parse_pipeline input = match parse_atat input with
    | (lhr, Lex.Pipeline :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_expr rhr in (Pipeline (lhr, rhr), remain)
    | (lhr, Lex.Pipeline :: rhr) -> begin match parse_pipeline rhr with
        | (Pipeline (rhrl, rhrr), remain) -> (Pipeline (Pipeline (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Pipeline (lhr, rhr), remain)
    end
    | x -> x
and parse_atat input = match parse_or input with
    | (lhr, Lex.AtAt :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_expr rhr in (App (lhr, rhr), remain)
    | (lhr, Lex.AtAt :: rhr) ->
        let (rhr, remain) = parse_atat rhr in (App (lhr, rhr), remain)
    | x -> x
and parse_or input = match parse_and input with
    | (lhr, Lex.Or :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_expr rhr in (Or (lhr, rhr), remain)
    | (lhr, Lex.Or :: rhr) -> begin match parse_or rhr with
        | (Or (rhrl, rhrr), remain) -> (Or (Or (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Or (lhr, rhr), remain)
    end
    | x -> x
and parse_and input = match parse_eq input with
    | (lhr, Lex.And :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_expr rhr in (And (lhr, rhr), remain)
    | (lhr, Lex.And :: rhr) -> begin match parse_and rhr with
        | (And (rhrl, rhrr), remain) -> (And (And (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (And (lhr, rhr), remain)
    end
    | x -> x
and parse_eq input = match parse_cons input with
    | (lhr, Lex.Eq :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_expr rhr in (Eq (lhr, rhr), remain)
    | (lhr, Lex.Eq :: rhr) -> begin match parse_eq rhr with
        | (Eq (rhrl, rhrr), remain) -> (Eq (Eq (lhr, rhrl), rhrr), remain)
        | (Neq (rhrl, rhrr), remain) -> (Neq (Eq (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Eq (lhr, rhr), remain)
    end
    | (lhr, Lex.Neq :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_expr rhr in (Neq (lhr, rhr), remain)
    | (lhr, Lex.Neq :: rhr) -> begin match parse_eq rhr with
        | (Eq (rhrl, rhrr), remain) -> (Eq (Neq (lhr, rhrl), rhrr), remain)
        | (Neq (rhrl, rhrr), remain) -> (Neq (Neq (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Neq (lhr, rhr), remain)
    end
    | (lhr, Lex.Gret :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_expr rhr in (Gret (lhr, rhr), remain)
    | (lhr, Lex.Gret :: rhr) ->
        let (rhr, remain) = parse_cons rhr in
        (Gret (lhr, rhr), remain)
    | (lhr, Lex.Less :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_expr rhr in (Less (lhr, rhr), remain)
    | (lhr, Lex.Less :: rhr) ->
        let (rhr, remain) = parse_cons rhr in
        (Less (lhr, rhr), remain)
    | x -> x
and parse_cons input = match parse_add input with
    | (lhr, Lex.Cons :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_expr rhr in (Cons (lhr, rhr), remain)
    | (lhr, Lex.Cons :: rhr) ->
        let (rhr, remain) = parse_cons rhr in
        Cons (lhr, rhr), remain
    | x -> x
and parse_add input = match parse_mul input with
    | (lhr, Lex.Add :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_expr rhr in (Add (lhr, rhr), remain)
    | (lhr, Lex.Add :: rhr) -> begin match parse_add rhr with
        | (Add (rhrl, rhrr), remain) -> (Add (Add (lhr, rhrl), rhrr), remain)
        | (Sub (rhrl, rhrr), remain) -> (Sub (Add (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Add (lhr, rhr), remain)
    end
    | (lhr, Lex.Sub :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_expr rhr in (Sub (lhr, rhr), remain)
    | (lhr, Lex.Sub :: rhr) -> begin match parse_add rhr with
        | (Add (rhrl, rhrr), remain) -> (Add (Sub (lhr, rhrl), rhrr), remain)
        | (Sub (rhrl, rhrr), remain) -> (Sub (Sub (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Sub (lhr, rhr), remain)
    end
    | x -> x
and parse_mul input = match parse_unary input with
    | (lhr, Lex.Mul :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_expr rhr in (Mul (lhr, rhr), remain)
    | (lhr, Lex.Mul :: rhr) -> begin match parse_mul rhr with
        | (Mul (rhrl, rhrr), remain) -> (Mul (Mul (lhr, rhrl), rhrr), remain)
        | (Div (rhrl, rhrr), remain) -> (Div (Mul (lhr, rhrl), rhrr), remain)
        | (Mod (rhrl, rhrr), remain) -> (Mod (Mul (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Mul (lhr, rhr), remain)
    end
    | (lhr, Lex.Div :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_expr rhr in (Div (lhr, rhr), remain)
    | (lhr, Lex.Div :: rhr) -> begin match parse_mul rhr with
        | (Mul (rhrl, rhrr), remain) -> (Mul (Div (lhr, rhrl), rhrr), remain)
        | (Div (rhrl, rhrr), remain) -> (Div (Div (lhr, rhrl), rhrr), remain)
        | (Mod (rhrl, rhrr), remain) -> (Mod (Div (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Div (lhr, rhr), remain)
    end
    | (lhr, Lex.Mod :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_expr rhr in (Mod (lhr, rhr), remain)
    | (lhr, Lex.Mod :: rhr) -> begin match parse_mul rhr with
        | (Mul (rhrl, rhrr), remain) -> (Mul (Mod (lhr, rhrl), rhrr), remain)
        | (Div (rhrl, rhrr), remain) -> (Div (Mod (lhr, rhrl), rhrr), remain)
        | (Mod (rhrl, rhrr), remain) -> (Mod (Mod (lhr, rhrl), rhrr), remain)
        | (rhr, remain) -> (Mod (lhr, rhr), remain)
    end
    | x -> x
and parse_unary = function
    | Lex.Sub :: remain when succ_lets remain ->
        let (exp, remain) = parse_expr remain in
        (Neg exp ,remain)
    | Lex.Sub :: remain ->
        let (exp, remain) = parse_unary remain in
        (Neg exp ,remain)
    | Lex.Not :: remain when succ_lets remain ->
        let (exp, remain) = parse_expr remain in
        (Not exp ,remain)
    | Lex.Not :: remain ->
        let (exp, remain) = parse_unary remain in
        (Not exp ,remain)
    | Lex.Ref :: remain when succ_lets remain ->
        let (exp, remain) = parse_expr remain in
        (Ref exp ,remain)
    | Lex.Ref :: remain ->
        let (exp, remain) = parse_unary remain in
        (Ref exp ,remain)
    | input -> parse_app input
and parse_app input =
    let nexts_term = function
        | Lex.LIdent _ :: _ -> true
        | Lex.UIdent _ :: _ -> true
        | Lex.Int _ :: _ -> true
        | Lex.True :: _ -> true
        | Lex.False :: _ -> true
        | Lex.LP :: _ -> true
        | Lex.LB :: _ -> true
        | _ -> false
    in
    let reverse tree =
        let rec dec = function
            | App (f, arg) -> f :: dec arg
            | t -> [t]
        in match dec tree with
        | [] -> raise @@ Failure "internal error"
        | f :: args -> List.fold_left (fun f arg -> App(f, arg)) f args
    in
    let rec internal input = match parse_array_access input with
        | (f, remain) when nexts_term remain ->
            begin match internal remain with
            | (arg, remain) -> App (f, arg), remain
            end
        | x -> x
    in
    let (tree, remain) = internal input in (reverse tree, remain)
and parse_array_access input = match parse_term input with
    | (lhr, Lex.Dot :: Lex.LP :: remain) -> begin match parse_expr remain with
        | (rhr, Lex.RP :: remain) -> (Index (lhr, rhr), remain)
        | x -> raise @@ SyntaxError (Printf.sprintf "paren is not balanced %s " @@ show_parsed_t x)
    end
    | x -> x
and parse_term = function
    | Lex.UIdent _ :: _ as remain ->
        let (id, remain) = parse_ident remain in
        (id, remain)
    | Lex.LIdent id :: remain -> (Var [id], remain)
    | Lex.Int i :: remain -> (Int i, remain)
    | Lex.True :: remain -> (Bool true, remain)
    | Lex.False :: remain -> (Bool false, remain)
    | Lex.LP :: Lex.RP :: remain -> (Tuple [], remain)
    | Lex.LB :: Lex.RB :: remain -> (Emp, remain)
    | Lex.LB :: remain -> begin match parse_list_elem remain with
            | (inner, Lex.RB :: remain) -> (inner, remain)
            | x -> raise @@ SyntaxError (Printf.sprintf "paren is not balanced %s " @@ show_parsed_t x)
        end
    | Lex.LP :: remain -> begin match parse_expr remain with
        | (inner, Lex.RP :: remain) -> (Paren inner, remain)
        | x -> raise @@ SyntaxError (Printf.sprintf "paren is not balanced %s " @@ show_parsed_t x)
    end
    | x -> (dbg_i x); raise @@ SyntaxError "term"
and parse_ident = function
    | Lex.UIdent pre :: Lex.Dot :: remain ->
        begin match parse_ident remain with
            | Var id, remain -> Var (pre :: id), remain
            | Ctor id, remain -> Ctor (pre :: id), remain
            | _ -> raise @@ Failure "internal error in parsing identifier"
        end
    | Lex.LIdent id :: remain -> (Var [id], remain)
    | Lex.UIdent id :: remain -> (Ctor [id], remain)
    | _ -> raise @@ SyntaxError "ident"
and parse_list_elem input = match parse_tuple input with
    | (lhr, Lex.Semicol :: Lex.RB :: remain) ->
        (Cons (lhr, Emp), Lex.RB :: remain)
    | (lhr, Lex.Semicol :: rhr) when succ_lets rhr ->
        let (rhr, remain ) = parse_tuple rhr in (Cons (lhr, Cons(rhr, Emp)), remain)
    | (lhr, Lex.Semicol :: rhr) -> begin match parse_list_elem rhr with
        | (Cons _ as rhr, remain) -> (Cons (lhr, rhr), remain)
        | (rhr, remain) -> (Cons (lhr, rhr), remain)
    end
    | (x, remain) -> (Cons(x, Emp), remain)
and parse_arms arm = match parse_pat arm with
    | (pat, Lex.Arrow :: expr) -> begin match parse_expr expr with
        | (expr, Lex.VBar :: remain) ->
            let (arms, remain) = parse_arms remain in
            ((pat, Bool true, expr) :: arms, remain)
        | (expr, remain) -> ([pat, Bool true, expr], remain)
        end
    | (pat, Lex.When :: when_e) -> begin match parse_expr when_e with
        | (when_e, Lex.Arrow :: expr) -> begin match parse_expr expr with
            | (expr, Lex.VBar :: remain) ->
                let (arms, remain) = parse_arms remain in
                ((pat, when_e, expr) :: arms, remain)
            | (expr, remain) -> ([pat, Bool true, expr], remain)
            end
        | _ -> raise @@ SyntaxError "invalid \'when\' guard"
        end
    | _ -> raise @@ SyntaxError "match arm"
and take_targs = function
    | Lex.TVar id :: remain ->
        let (targs, remain) = take_targs remain in
        (id :: targs, remain)
    | remain -> [], remain
and parse_let_ands input = begin match parse_params Lex.Eq input with
    | ([pat], Lex.Eq :: remain) ->
        begin match parse_expr remain with
        | (def, Lex.AndDef :: remain) ->
            let (ands, remain) = parse_let_ands remain in
            (pat, def) :: ands, remain
        | (def, remain) -> [pat, def], remain
        end
    | (PVar id :: args, Lex.Eq :: remain) ->
        begin match parse_expr remain with
        | (def, Lex.AndDef :: remain) ->
            let (ands, remain) = parse_let_ands remain in
            (PVar id, unfold_fun args def) :: ands, remain
        | (def, remain) -> [PVar id, unfold_fun args def], remain
        end
    | _ -> raise @@ SyntaxError "let stmt"
    end
and parse_letrec_ands input = begin match parse_params Lex.Eq input with
    | ([PVar id], Lex.Eq :: remain) ->
        begin match parse_expr remain with
        | (def, Lex.AndDef :: remain) ->
            let (ands, remain) = parse_letrec_ands remain in
            ([id], def) :: ands, remain
        | (def, remain) -> [[id], def], remain
        end
    | (PVar id :: args, Lex.Eq :: remain) ->
        begin match parse_expr remain with
        | (def, Lex.AndDef :: remain) ->
            let (ands, remain) = parse_letrec_ands remain in
            ([id], unfold_fun args def) :: ands, remain
        | (def, remain) -> [[id], unfold_fun args def], remain
        end
    | _ -> raise @@ SyntaxError "let stmt"
    end
and parse_type_ands = function
    | Lex.LIdent name :: remain ->
        begin match take_targs remain with
        | (targs, Lex.Eq :: remain) ->
            begin match parse_ty_variant remain with
                | (ty, Lex.AndDef :: remain) ->
                    let (ands, remain) = parse_type_ands remain in
                    (name, targs, ty) :: ands, remain
                | (ty, remain) ->
                    [name, targs, ty], remain
            end
        | _ -> raise @@ SyntaxError "and type"
        end
    | _ -> raise @@ SyntaxError "and type"

let parse input = match parse_expr input with
    | (ast, [Lex.Eof]) -> ast
    | x -> (dbg x); raise @@ SyntaxError "top"


let rec parse_stmts = function
    | Lex.Type :: Lex.LIdent name :: remain ->
        begin match take_targs remain with
        | (targs, Lex.Eq :: remain) ->
            begin match parse_ty_variant remain with
                | (ty, Lex.AndDef :: remain) ->
                    let (ands, remain) = parse_type_ands remain in
                    Type ((name, targs, ty) :: ands, parse_stmts remain)
                | (ty, remain) ->
                    Type ([name, targs, ty], parse_stmts remain)
            end
        | _ -> raise @@ SyntaxError "type def"
        end
    | Lex.Let :: Lex.Rec :: remain -> begin match parse_params Lex.Eq remain with
        | ([PVar id], Lex.Eq :: remain) ->
            begin match parse_expr remain with
            | (def, Lex.AndDef :: remain) ->
                let (ands, remain) = parse_letrec_ands remain in
                LetRec (([id], def) :: ands, parse_stmts remain)
            | (def, remain) ->
                LetRec ([[id], def], parse_stmts remain)
            end
        | (PVar id :: args, Lex.Eq :: remain) ->
            begin match parse_expr remain with
            | (def, Lex.AndDef :: remain) ->
                let (ands, remain) = parse_letrec_ands remain in
                LetRec (([id], unfold_fun args def) :: ands, parse_stmts remain)
            | (def, remain) ->
                LetRec ([[id], unfold_fun args def], parse_stmts remain)
            end
        | _ -> raise @@ SyntaxError "let stmt"
        end
    | Lex.Let :: remain -> begin match parse_params Lex.Eq remain with
        | ([pat], Lex.Eq :: remain) ->
            begin match parse_expr remain with
            | (def, Lex.AndDef :: remain) ->
                let (ands, remain) = parse_let_ands remain in
                Let ((pat, def) :: ands, parse_stmts remain)
            | (def, remain) ->
                Let ([pat, def], parse_stmts remain)
            end
        | (PVar id :: args, Lex.Eq :: remain) ->
            begin match parse_expr remain with
            | (def, Lex.AndDef :: remain) ->
                let (ands, remain) = parse_let_ands remain in
                Let ((PVar id, unfold_fun args def) :: ands, parse_stmts remain)
            | (def, remain) ->
                Let ([PVar id, unfold_fun args def], parse_stmts remain)
            end
        | _ -> raise @@ SyntaxError "let stmt"
        end
    | Lex.Eof :: [] -> Never
    | _ -> raise @@ SyntaxError "stmt"

let f fname src = parse_stmts @@ Lex.f fname src
