type ty_t =
    | TInt of Lex.pos_t
    | TBool of Lex.pos_t
    | TString of Lex.pos_t
    | TParen of ty_t
    | TVar of string * Lex.pos_t
    | TTuple of ty_t list * Lex.pos_t
    | TApp of ty_t list * string list * Lex.pos_t
[@@deriving show]

type tydef_t =
    | Variant of (string * Lex.pos_t * ty_t list) list
    | Alias of ty_t
[@@deriving show]

type pat_t =
    | PInt of int * Lex.pos_t
    | PBool of bool * Lex.pos_t
    | PVar of string * Lex.pos_t
    | PEmp of Lex.pos_t
    | PCons of pat_t * pat_t * Lex.pos_t
    | PTuple of pat_t list * Lex.pos_t
    | PParen of pat_t
    | PCtor of string list * Lex.pos_t
    | PCtorApp of string list * pat_t * Lex.pos_t
    | PAs of pat_t list * Lex.pos_t
    | POr of pat_t * pat_t list * Lex.pos_t
[@@deriving show]

type t =
    | Never
    | Emp of Lex.pos_t
    | Int of int * Lex.pos_t
    | Bool of bool * Lex.pos_t
    | Var of string list * Lex.pos_t
    | Ctor of string list * Lex.pos_t
    | Index of t * t * Lex.pos_t
    | Assign of t * t * Lex.pos_t
    | ArrayAssign of t * t * t * Lex.pos_t
    | Add of t * t * Lex.pos_t
    | Sub of t * t * Lex.pos_t
    | Mul of t * t * Lex.pos_t
    | Div of t * t * Lex.pos_t
    | Mod of t * t * Lex.pos_t
    | Neg of t * Lex.pos_t
    | Eq of t * t * Lex.pos_t
    | Neq of t * t * Lex.pos_t
    | Or of t * t * Lex.pos_t
    | And of t * t * Lex.pos_t
    | Gret of t * t * Lex.pos_t
    | Less of t * t * Lex.pos_t
    | Cons of t * t * Lex.pos_t
    | Tuple of t list * Lex.pos_t
    | If of t * t * t * Lex.pos_t
    | Let of (pat_t * t) list * t
    | LetRec of (string list * Lex.pos_t * t) list * t
    | Type of
        (string * Lex.pos_t * (string * Lex.pos_t) list * tydef_t) list * t
    | Fun of (string * Lex.pos_t) list * t
    | Match of t * (pat_t * t * t) list
    | App of t * t list * Lex.pos_t
    | Seq of t * t * Lex.pos_t
    | Pipeline of t * t * Lex.pos_t
    | Paren of t
[@@deriving show]

type input_t = (Lex.t * Lex.pos_t) list [@@deriving show]

type parsed_t = t * Lex.pos_t * input_t [@@deriving show]

exception SyntaxError of string

let rec split_with f = function
    | hd :: tl -> (
        if f hd then Some ([], tl)
        else
          match split_with f tl with
          | Some (first_half, second_half) ->
              Some (hd :: first_half, second_half)
          | None -> None )
    | [] -> None

let dbg ast = print_endline @@ show_parsed_t ast

let dbg_i ast = print_endline @@ show_input_t ast

let count = ref 0

let gen_fresh () =
    count := 1 + !count ;
    "<anonymous" ^ string_of_int !count ^ ">"

let succ_lets = function
    | (Lex.Let, _) :: _ -> true
    | (Lex.Fun, _) :: _ -> true
    | (Lex.Match, _) :: _ -> true
    | (Lex.If, _) :: _ -> true
    | _ -> false

let rec take_params = function
    | Lex.LIdent id :: remain ->
        let params, remain = take_params remain in
        (id :: params, remain)
    | remain -> ([], remain)

type param_taken_t = string list * input_t [@@deriving show]

let rec parse_ty input = parse_ty_tuple input

and parse_ty_tuple input =
    match parse_tapp input with
    | lhr, p, (Lex.Mul, _) :: remain -> (
      match parse_ty_tuple remain with
      | TTuple (rhr, _), _, remain -> (TTuple (lhr :: rhr, p), p, remain)
      | rhr, _, remain -> (TTuple ([lhr; rhr], p), p, remain) )
    | t -> t

and parse_variant_right input =
    match parse_tapp input with
    | lhr, p, (Lex.Mul, _) :: remain ->
        let rhr, _, remain = parse_variant_right remain in
        (lhr :: rhr, p, remain)
    | t, p, remain -> ([t], p, remain)

and parse_tid = function
    | (Lex.LIdent id, p) :: remain -> ([id], p, remain)
    | (Lex.UIdent id, p) :: (Lex.Dot, _) :: remain ->
        let last, _, remain = parse_tid remain in
        (id :: last, p, remain)
    | _ -> raise @@ SyntaxError "syntax error while reading type id"

and parse_tapp_arg =
    let rec parse_tapp_args input =
        match parse_ty_term input with
        | lhr, p, (Lex.Comma, _) :: remain ->
            let rhr, p, remain = parse_tapp_args remain in
            (lhr :: rhr, p, remain)
        | t, p, remain -> ([t], p, remain)
    in
    function
    | (Lex.LP, _) :: inner -> (
      match parse_tapp inner with
      | _, _, (Lex.Mul, _) :: _ -> (
        match parse_ty_tuple inner with
        | ty, p, (Lex.RP, _) :: remain -> ([TParen ty], p, remain)
        | _ ->
            raise
            @@ SyntaxError
                 "syntax error: paren isn't balanced (reading type tuple)" )
      | _, p, (Lex.Comma, _) :: _ -> (
        match parse_tapp_args inner with
        | tys, p, (Lex.RP, _) :: remain -> (tys, p, remain)
        | _ ->
            raise
            @@ SyntaxError
                 "syntax error: paren isn't balanced (reading type contructor)"
        )
      | ty, p, (Lex.RP, _) :: remain -> ([ty], p, remain)
      | _ ->
          raise
          @@ SyntaxError "syntax error: paren isn't balanced (reading type)" )
    | input ->
        let ty, p, remain = parse_ty_term input in
        ([ty], p, remain)

and parse_tapp input =
    let rec f t p input =
        match (t, p, input) with
        | t, _, ((Lex.LIdent _, _) :: _ as remain) ->
            let id, _, remain = parse_tid remain in
            f (TApp ([t], id, p)) p remain
        | t, _, ((Lex.UIdent _, _) :: _ as remain) ->
            let id, _, remain = parse_tid remain in
            f (TApp ([t], id, p)) p remain
        | r -> r
    in
    match parse_tapp_arg input with
    | ts, p, ((Lex.LIdent _, _) :: _ as remain) ->
        let id, _, remain = parse_tid remain in
        f (TApp (ts, id, p)) p remain
    | ts, p, ((Lex.UIdent _, _) :: _ as remain) ->
        let id, _, remain = parse_tid remain in
        f (TApp (ts, id, p)) p remain
    | [t], p, remain -> (t, p, remain)
    | _ -> raise @@ SyntaxError "syntax error"

and parse_ty_term = function
    | (Lex.TVar id, p) :: remain -> (TVar (id, p), p, remain)
    | (Lex.TInt, p) :: remain -> (TInt p, p, remain)
    | (Lex.TBool, p) :: remain -> (TBool p, p, remain)
    | (Lex.TString, p) :: remain -> (TString p, p, remain)
    | (Lex.LIdent _, p) :: _ as input ->
        let id, p, remain = parse_tid input in
        (TApp ([], id, p), p, remain)
    | (Lex.UIdent _, p) :: _ as input ->
        let id, p, remain = parse_tid input in
        (TApp ([], id, p), p, remain)
    | (Lex.LP, _) :: inner -> (
      match parse_ty inner with
      | inner, p, (Lex.RP, _) :: remain -> (TParen inner, p, remain)
      | _ -> raise @@ SyntaxError "paren is not balanced in type" )
    | t -> raise @@ SyntaxError (Printf.sprintf "ty_term %s" @@ show_input_t t)

let rec parse_ty_variant = function
    | (Lex.VBar, _) :: (Lex.UIdent name, pn) :: (Lex.Of, _) :: remain -> (
      match parse_variant_right remain with
      | t, _, (Lex.VBar, _) :: arms -> (
        match parse_ty_variant arms with
        | Variant arms, _, remain ->
            (Variant ((name, pn, t) :: arms), pn, remain)
        | _ -> raise @@ SyntaxError "variant" )
      | t, _, remain -> (Variant [(name, pn, t)], pn, remain) )
    | (Lex.UIdent name, pn) :: (Lex.Of, _) :: remain -> (
      match parse_variant_right remain with
      | t, _, (Lex.VBar, _) :: arms -> (
        match parse_ty_variant arms with
        | Variant arms, _, remain ->
            (Variant ((name, pn, t) :: arms), pn, remain)
        | _ -> raise @@ SyntaxError "variant" )
      | t, _, remain -> (Variant [(name, pn, t)], pn, remain) )
    | (Lex.VBar, _) :: (Lex.UIdent name, pn) :: (Lex.VBar, _) :: arms -> (
      match parse_ty_variant arms with
      | Variant arms, _, remain -> (Variant ((name, pn, []) :: arms), pn, remain)
      | _ -> raise @@ SyntaxError "variant" )
    | (Lex.VBar, _) :: (Lex.UIdent name, pn) :: remain ->
        (Variant [(name, pn, [])], pn, remain)
    | (Lex.UIdent name, pn) :: (Lex.VBar, _) :: arms -> (
      match parse_ty_variant arms with
      | Variant arms, _, remain -> (Variant ((name, pn, []) :: arms), pn, remain)
      | _ -> raise @@ SyntaxError "variant" )
    | (Lex.UIdent name, pn) :: remain -> (Variant [(name, pn, [])], pn, remain)
    | input ->
        let ty, p, remain = parse_ty input in
        (Alias ty, p, remain)

let rec parse_pat input =
    (* TODO or *)
    match parse_pat_tuple input with
    | pat, p, (Lex.As, _) :: rhr -> (
      match parse_pat rhr with
      | PAs (tp, _), _, remain -> (PAs (pat :: tp, p), p, remain)
      | rhr, _, remain -> (PAs ([pat; rhr], p), p, remain) )
    | p -> p

and parse_pat_tuple input =
    match parse_pat_cons input with
    | pat, p, (Lex.Comma, _) :: rhr -> (
      match parse_pat_tuple rhr with
      | PTuple (tp, _), _, remain -> (PTuple (pat :: tp, p), p, remain)
      | rhr, _, remain -> (PTuple ([pat; rhr], p), p, remain) )
    | p -> p

and parse_pat_cons input =
    match parse_pat_term input with
    | pat, p, (Lex.Cons, _) :: rhr ->
        let rhr, _, remain = parse_pat_cons rhr in
        (PCons (pat, rhr, p), p, remain)
    | p -> p

and parse_pat_term = function
    | (Lex.LIdent id, p) :: remain -> (PVar (id, p), p, remain)
    | (Lex.UIdent _, p) :: _ as remain -> (
      match parse_pat_ctor remain with
      | id, _, ((Lex.Cons, _) as hd) :: remain ->
          (PCtor (id, p), p, hd :: remain)
      | id, _, ((Lex.Comma, _) as hd) :: remain ->
          (PCtor (id, p), p, hd :: remain)
      | id, _, ((Lex.Arrow, _) as hd) :: remain ->
          (PCtor (id, p), p, hd :: remain)
      | id, _, ((Lex.Eq, _) as hd) :: remain -> (PCtor (id, p), p, hd :: remain)
      | id, _, remain ->
          let arg, _, remain = parse_pat_term remain in
          (PCtorApp (id, arg, p), p, remain) )
    | (Lex.Int i, p) :: remain -> (PInt (i, p), p, remain)
    | (Lex.True, p) :: remain -> (PBool (true, p), p, remain)
    | (Lex.False, p) :: remain -> (PBool (false, p), p, remain)
    | (Lex.LP, p) :: (Lex.RP, _) :: remain -> (PTuple ([], p), p, remain)
    | (Lex.LB, p) :: (Lex.RB, _) :: remain -> (PEmp p, p, remain)
    | (Lex.LB, _) :: remain -> (
      match parse_pat_list_elem remain with
      | inner, p, (Lex.RB, _) :: remain -> (inner, p, remain)
      | x -> raise @@ SyntaxError "paren is not balanced in pattern" )
    | (Lex.LP, _) :: inner -> (
      match parse_pat inner with
      | inner, p, (Lex.RP, _) :: remain -> (PParen inner, p, remain)
      | _ -> raise @@ SyntaxError "paren is not balanced in pattern" )
    | x ->
        raise @@ SyntaxError (Printf.sprintf "pattern term %s" @@ show_input_t x)

and parse_pat_ctor = function
    | (Lex.UIdent id, p) :: (Lex.Dot, _) :: remain ->
        let last, _, remain = parse_pat_ctor remain in
        (id :: last, p, remain)
    | (Lex.UIdent id, p) :: remain -> ([id], p, remain)
    | _ -> raise @@ SyntaxError "ident"

and parse_pat_list_elem input =
    match parse_pat input with
    | lhr, p, (Lex.Semicol, _) :: ((Lex.RB, _) as hd) :: remain ->
        (PCons (lhr, PEmp p, p), p, hd :: remain)
    (*| lhr, p, (Lex.Semicol, _) :: rhr when succ_lets rhr -> let rhr, p',
      remain = parse_pat rhr in (PCons (lhr, PCons (rhr, PEmp p', p'), p), p,
      remain)*)
    | lhr, p, (Lex.Semicol, _) :: rhr -> (
      match parse_pat_list_elem rhr with
      | (PCons _ as rhr), p, remain -> (PCons (lhr, rhr, p), p, remain)
      | rhr, p, remain -> (PCons (lhr, rhr, p), p, remain) )
    | x, p, remain -> (PCons (x, PEmp p, p), p, remain)

let rec parse_params stop input =
    match parse_pat input with
    | pat, p, ((sym, _) as hd) :: remain when sym = stop ->
        ([(pat, p)], hd :: remain)
    | pat, p, remain ->
        let pats, remain = parse_params stop remain in
        ((pat, p) :: pats, remain)

let rec parse_expr = function
    | (Lex.Let, p) :: (Lex.Rec, _) :: remain -> (
      match parse_letrec_ands remain with
      | defs, (Lex.In, _) :: remain ->
          let expr, p, remain = parse_expr remain in
          (LetRec (defs, expr), p, remain)
      | _ -> raise @@ SyntaxError "letrec expr" )
    | (Lex.Let, p) :: remain -> (
      match parse_let_ands remain with
      | defs, (Lex.In, _) :: remain ->
          let expr, p, remain = parse_expr remain in
          (Let (defs, expr), p, remain)
      | _ -> raise @@ SyntaxError "let expr" )
    | (Lex.Fun, p) :: remain -> (
      match parse_params Lex.Arrow remain with
      | args, (Lex.Arrow, _) :: remain ->
          let expr, p, remain = parse_expr remain in
          (unfold_fun args expr, p, remain)
      | x -> raise @@ SyntaxError "fun" )
    | (Lex.If, p) :: cond -> (
      match parse_expr cond with
      | cond, _, (Lex.Then, _) :: then_e -> (
        match parse_expr then_e with
        | then_e, _, (Lex.Else, _) :: else_e ->
            let else_e, _, remain = parse_expr else_e in
            (If (cond, then_e, else_e, p), p, remain)
        | _ -> raise @@ SyntaxError "if: else not found" )
      | _ -> raise @@ SyntaxError "if: then not found" )
    | (Lex.Match, p) :: remain -> (
      match parse_expr remain with
      | target, _, (Lex.With, _) :: (Lex.VBar, _) :: arms ->
          let arms, _, remain = parse_arms arms in
          (Match (target, arms), p, remain)
      | target, _, (Lex.With, _) :: arms ->
          let arms, _, remain = parse_arms arms in
          (Match (target, arms), p, remain)
      | _ -> raise @@ SyntaxError "match" )
    | others -> parse_seq others

and unfold_fun args expr =
    let body, params =
        List.fold_left
          (fun (inner, params) -> function
            | PVar (id, _), p -> (inner, (id, p) :: params)
            | pat, p ->
                let tmpname = gen_fresh () in
                ( Match (Var ([tmpname], p), [(pat, Bool (true, p), inner)])
                , (tmpname, p) :: params ))
          (expr, []) (List.rev args)
    in
    Fun (params, body)

and parse_seq input =
    match parse_assign input with
    | lhr, p, (Lex.Semicol, p_op) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_assign rhr in
        (Seq (lhr, rhr, p_op), p, remain)
    | lhr, p, (Lex.Semicol, p_op) :: rhr ->
        let rhr, _, remain = parse_seq rhr in
        (Seq (lhr, rhr, p_op), p, remain)
    | x -> x

and parse_assign input =
    match parse_arrayassign input with
    | lhr, p, (Lex.Assign, p_op) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_tuple rhr in
        (Assign (lhr, rhr, p_op), p, remain)
    | lhr, p, (Lex.Assign, p_op) :: rhr ->
        let rhr, _, remain = parse_tuple rhr in
        (Assign (lhr, rhr, p_op), p, remain)
    | x -> x

and parse_arrayassign input =
    match parse_tuple input with
    | Index (arr, idx, _), p, (Lex.ArrayAssign, p_op) :: rhr when succ_lets rhr
      ->
        let rhr, _, remain = parse_tuple rhr in
        (ArrayAssign (arr, idx, rhr, p), p, remain)
    | Index (arr, idx, _), p, (Lex.ArrayAssign, p_op) :: rhr ->
        let rhr, _, remain = parse_tuple rhr in
        (ArrayAssign (arr, idx, rhr, p_op), p, remain)
    | x -> x

and parse_tuple input =
    match parse_pipeline input with
    | lhr, p, (Lex.Comma, _) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_expr rhr in
        (Tuple ([lhr; rhr], p), p, remain)
    | lhr, p, (Lex.Comma, _) :: rhr -> (
      match parse_tuple rhr with
      | Tuple (rhr, _), p, remain -> (Tuple (lhr :: rhr, p), p, remain)
      | rhr, _, remain -> (Tuple ([lhr; rhr], p), p, remain) )
    | x -> x

and parse_pipeline input =
    match parse_atat input with
    | lhr, p, (Lex.Pipeline, p_op) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_expr rhr in
        (Pipeline (lhr, rhr, p_op), p, remain)
    | lhr, p, (Lex.Pipeline, p_op) :: rhr -> (
      match parse_pipeline rhr with
      | Pipeline (rhrl, rhrr, p_op'), _, remain ->
          (Pipeline (Pipeline (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | rhr, _, remain -> (Pipeline (lhr, rhr, p_op), p, remain) )
    | x -> x

and parse_atat input =
    match parse_or input with
    | lhr, p, (Lex.AtAt, p_op) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_expr rhr in
        (App (lhr, [rhr], p_op), p, remain)
    | lhr, p, (Lex.AtAt, p_op) :: rhr ->
        let rhr, _, remain = parse_atat rhr in
        (App (lhr, [rhr], p_op), p, remain)
    | x -> x

and parse_or input =
    match parse_and input with
    | lhr, p, (Lex.Or, p_op) :: rhr when succ_lets rhr ->
        let rhr, p', remain = parse_expr rhr in
        (Or (lhr, rhr, p_op), p, remain)
    | lhr, p, (Lex.Or, p_op) :: rhr -> (
      match parse_or rhr with
      | Or (rhrl, rhrr, p'), _, remain ->
          (Or (Or (lhr, rhrl, p_op), rhrr, p'), p, remain)
      | rhr, _, remain -> (Or (lhr, rhr, p), p, remain) )
    | x -> x

and parse_and input =
    match parse_eq input with
    | lhr, p, (Lex.And, p_op) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_expr rhr in
        (And (lhr, rhr, p_op), p, remain)
    | lhr, p, (Lex.And, p_op) :: rhr -> (
      match parse_and rhr with
      | And (rhrl, rhrr, p_op'), _, remain ->
          (And (And (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | rhr, _, remain -> (And (lhr, rhr, p_op), p, remain) )
    | x -> x

(* TODO eqとneqはnonassoc *)
and parse_eq input =
    match parse_cons input with
    | lhr, p, (Lex.Eq, p_op) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_expr rhr in
        (Eq (lhr, rhr, p_op), p, remain)
    | lhr, p, (Lex.Eq, p_op) :: rhr -> (
      match parse_eq rhr with
      | Eq (rhrl, rhrr, p_op'), _, remain ->
          (Eq (Eq (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | Neq (rhrl, rhrr, p_op'), _, remain ->
          (Neq (Eq (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | rhr, _, remain -> (Eq (lhr, rhr, p_op), p, remain) )
    | lhr, p, (Lex.Neq, p_op) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_expr rhr in
        (Neq (lhr, rhr, p_op), p, remain)
    | lhr, p, (Lex.Neq, p_op) :: rhr -> (
      match parse_eq rhr with
      | Eq (rhrl, rhrr, p_op'), _, remain ->
          (Eq (Neq (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | Neq (rhrl, rhrr, p_op'), _, remain ->
          (Neq (Neq (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | rhr, _, remain -> (Neq (lhr, rhr, p_op), p, remain) )
    | lhr, p, (Lex.Gret, p_op) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_expr rhr in
        (Gret (lhr, rhr, p_op), p, remain)
    | lhr, p, (Lex.Gret, p_op) :: rhr ->
        let rhr, _, remain = parse_cons rhr in
        (Gret (lhr, rhr, p_op), p, remain)
    | lhr, p, (Lex.Less, p_op) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_expr rhr in
        (Less (lhr, rhr, p_op), p, remain)
    | lhr, p, (Lex.Less, p_op) :: rhr ->
        let rhr, _, remain = parse_cons rhr in
        (Less (lhr, rhr, p_op), p, remain)
    | x -> x

and parse_cons input =
    match parse_add input with
    | lhr, p, (Lex.Cons, p_op) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_expr rhr in
        (Cons (lhr, rhr, p_op), p, remain)
    | lhr, p, (Lex.Cons, p_op) :: rhr ->
        let rhr, _, remain = parse_cons rhr in
        (Cons (lhr, rhr, p_op), p, remain)
    | x -> x

and parse_add input =
    match parse_mul input with
    | lhr, p, (Lex.Add, p_op) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_expr rhr in
        (Add (lhr, rhr, p_op), p, remain)
    | lhr, p, (Lex.Add, p_op) :: rhr -> (
      match parse_add rhr with
      | Add (rhrl, rhrr, p_op'), _, remain ->
          (Add (Add (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | Sub (rhrl, rhrr, p_op'), _, remain ->
          (Sub (Add (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | rhr, _, remain -> (Add (lhr, rhr, p_op), p, remain) )
    | lhr, p, (Lex.Sub, p_op) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_expr rhr in
        (Sub (lhr, rhr, p_op), p, remain)
    | lhr, p, (Lex.Sub, p_op) :: rhr -> (
      match parse_add rhr with
      | Add (rhrl, rhrr, p_op'), p, remain ->
          (Add (Sub (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | Sub (rhrl, rhrr, p_op'), p, remain ->
          (Sub (Sub (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | rhr, _, remain -> (Sub (lhr, rhr, p_op), p, remain) )
    | x -> x

and parse_mul input =
    match parse_unary input with
    | lhr, p, (Lex.Mul, p_op) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_expr rhr in
        (Mul (lhr, rhr, p_op), p, remain)
    | lhr, p, (Lex.Mul, p_op) :: rhr -> (
      match parse_mul rhr with
      | Mul (rhrl, rhrr, p_op'), _, remain ->
          (Mul (Mul (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | Div (rhrl, rhrr, p_op'), _, remain ->
          (Div (Mul (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | Mod (rhrl, rhrr, p_op'), _, remain ->
          (Mod (Mul (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | rhr, _, remain -> (Mul (lhr, rhr, p_op), p, remain) )
    | lhr, p, (Lex.Div, p_op) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_expr rhr in
        (Div (lhr, rhr, p_op), p, remain)
    | lhr, p, (Lex.Div, p_op) :: rhr -> (
      match parse_mul rhr with
      | Mul (rhrl, rhrr, p_op'), _, remain ->
          (Mul (Div (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | Div (rhrl, rhrr, p_op'), _, remain ->
          (Div (Div (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | Mod (rhrl, rhrr, p_op'), _, remain ->
          (Mod (Div (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | rhr, _, remain -> (Div (lhr, rhr, p_op), p, remain) )
    | lhr, p, (Lex.Mod, p_op) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_expr rhr in
        (Mod (lhr, rhr, p_op), p, remain)
    | lhr, p, (Lex.Mod, p_op) :: rhr -> (
      match parse_mul rhr with
      | Mul (rhrl, rhrr, p_op'), _, remain ->
          (Mul (Mod (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | Div (rhrl, rhrr, p_op'), _, remain ->
          (Div (Mod (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | Mod (rhrl, rhrr, p_op'), _, remain ->
          (Mod (Mod (lhr, rhrl, p_op), rhrr, p_op'), p, remain)
      | rhr, p, remain -> (Mod (lhr, rhr, p_op), p, remain) )
    | x -> x

and parse_unary = function
    | (Lex.Sub, p) :: remain when succ_lets remain ->
        let exp, _, remain = parse_expr remain in
        (Neg (exp, p), p, remain)
    | (Lex.Sub, p) :: remain ->
        let exp, _, remain = parse_unary remain in
        (Neg (exp, p), p, remain)
    | input -> parse_app input

and parse_app input =
    let nexts_term = function
        | (Lex.LIdent _, _) :: _ -> true
        | (Lex.UIdent _, _) :: _ -> true
        | (Lex.Int _, _) :: _ -> true
        | (Lex.True, _) :: _ -> true
        | (Lex.False, _) :: _ -> true
        | (Lex.LP, _) :: _ -> true
        | (Lex.LB, _) :: _ -> true
        | _ -> false
    in
    let rec take_args input =
        match parse_array_access input with
        | arg, p, remain when nexts_term remain ->
            let args, remain = take_args remain in
            ((arg, p) :: args, remain)
        | arg, p, remain -> ([(arg, p)], remain)
    in
    match take_args input with
    | [(x, p)], remain -> (x, p, remain)
    | (f, p) :: args, remain -> (App (f, List.map fst args, p), p, remain)
    | [], _ -> raise @@ SyntaxError "function app"

and parse_array_access input =
    match parse_term input with
    | lhr, p, (Lex.Dot, _) :: (Lex.LP, _) :: remain -> (
      match parse_expr remain with
      | rhr, _, (Lex.RP, _) :: remain -> (Index (lhr, rhr, p), p, remain)
      | x ->
          raise
          @@ SyntaxError
               (Printf.sprintf "paren is not balanced %s " @@ show_parsed_t x) )
    | x -> x

and parse_term = function
    | (Lex.UIdent _, p) :: _ as remain ->
        let id, _, remain = parse_ident remain in
        (id, p, remain)
    | (Lex.LIdent id, p) :: remain -> (Var ([id], p), p, remain)
    | (Lex.Int i, p) :: remain -> (Int (i, p), p, remain)
    | (Lex.True, p) :: remain -> (Bool (true, p), p, remain)
    | (Lex.False, p) :: remain -> (Bool (false, p), p, remain)
    | (Lex.LP, p) :: (Lex.RP, _) :: remain -> (Tuple ([], p), p, remain)
    | (Lex.LB, p) :: (Lex.RB, _) :: remain -> (Emp p, p, remain)
    | (Lex.LB, _) :: remain -> (
      match parse_list_elem remain with
      | inner, p, (Lex.RB, _) :: remain -> (inner, p, remain)
      | x ->
          raise
          @@ SyntaxError
               (Printf.sprintf "paren is not balanced %s " @@ show_parsed_t x) )
    | (Lex.LP, _) :: remain -> (
      match parse_expr remain with
      | inner, p, (Lex.RP, _) :: remain -> (Paren inner, p, remain)
      | x ->
          raise
          @@ SyntaxError
               (Printf.sprintf "paren is not balanced %s " @@ show_parsed_t x) )
    | x ->
        dbg_i x ;
        raise @@ SyntaxError "term"

and parse_ident = function
    | (Lex.UIdent pre, p) :: (Lex.Dot, _) :: remain -> (
      match parse_ident remain with
      | Var (id, _), _, remain -> (Var (pre :: id, p), p, remain)
      | Ctor (id, _), _, remain -> (Ctor (pre :: id, p), p, remain)
      | _ -> raise @@ Failure "internal error in parsing identifier" )
    | (Lex.LIdent id, p) :: remain -> (Var ([id], p), p, remain)
    | (Lex.UIdent id, p) :: remain -> (Ctor ([id], p), p, remain)
    | _ -> raise @@ SyntaxError "ident"

and parse_list_elem input =
    match parse_tuple input with
    | lhr, p, (Lex.Semicol, p_op) :: ((Lex.RB, _) as hd) :: remain ->
        (Cons (lhr, Emp p_op, p_op), p, hd :: remain)
    | lhr, p, (Lex.Semicol, p_op) :: rhr when succ_lets rhr ->
        let rhr, _, remain = parse_tuple rhr in
        (Cons (lhr, Cons (rhr, Emp p_op, p_op), p_op), p, remain)
    | lhr, p, (Lex.Semicol, p_op) :: rhr -> (
      match parse_list_elem rhr with
      | (Cons _ as rhr), _, remain -> (Cons (lhr, rhr, p_op), p, remain)
      | rhr, _, remain -> (Cons (lhr, rhr, p_op), p, remain) )
    | x, p, remain -> (Cons (x, Emp p, p), p, remain)

and parse_arms arm =
    match parse_pat arm with
    | pat, p, (Lex.Arrow, _) :: expr -> (
      match parse_expr expr with
      | expr, _, (Lex.VBar, _) :: remain ->
          let arms, _, remain = parse_arms remain in
          ((pat, Bool (true, p), expr) :: arms, p, remain)
      | expr, p, remain -> ([(pat, Bool (true, p), expr)], p, remain) )
    | pat, p, (Lex.When, _) :: when_e -> (
      match parse_expr when_e with
      | when_e, p, (Lex.Arrow, p_arm) :: expr -> (
        match parse_expr expr with
        | expr, _, (Lex.VBar, _) :: remain ->
            let arms, _, remain = parse_arms remain in
            ((pat, when_e, expr) :: arms, p_arm, remain)
        | expr, _, remain -> ([(pat, Bool (true, p_arm), expr)], p_arm, remain)
        )
      | _ -> raise @@ SyntaxError "invalid \'when\' guard" )
    | _ -> raise @@ SyntaxError "match arm"

and take_targs = function
    | (Lex.TVar id, p) :: (Lex.Comma, _) :: remain ->
        let targs, remain = take_targs remain in
        ((id, p) :: targs, remain)
    | (Lex.TVar id, p) :: remain -> ([(id, p)], remain)
    | _ -> raise @@ SyntaxError "targs"

and parse_let_ands input =
    match parse_params Lex.Eq input with
    | [(pat, _)], (Lex.Eq, _) :: remain -> (
      match parse_expr remain with
      | def, _, (Lex.AndDef, _) :: remain ->
          let ands, remain = parse_let_ands remain in
          ((pat, def) :: ands, remain)
      | def, _, remain -> ([(pat, def)], remain) )
    | (PVar (id, p), _) :: args, (Lex.Eq, _) :: remain -> (
      match parse_expr remain with
      | def, _, (Lex.AndDef, _) :: remain ->
          let ands, remain = parse_let_ands remain in
          ((PVar (id, p), unfold_fun args def) :: ands, remain)
      | def, _, remain -> ([(PVar (id, p), unfold_fun args def)], remain) )
    | _ -> raise @@ SyntaxError "let stmt"

and parse_letrec_ands input =
    match parse_params Lex.Eq input with
    | [(PVar (id, p), _)], (Lex.Eq, _) :: remain -> (
      match parse_expr remain with
      | def, _, (Lex.AndDef, _) :: remain ->
          let ands, remain = parse_letrec_ands remain in
          (([id], p, def) :: ands, remain)
      | def, _, remain -> ([([id], p, def)], remain) )
    | (PVar (id, p), _) :: args, (Lex.Eq, _) :: remain -> (
      match parse_expr remain with
      | def, _, (Lex.AndDef, _) :: remain ->
          let ands, remain = parse_letrec_ands remain in
          (([id], p, unfold_fun args def) :: ands, remain)
      | def, _, remain -> ([([id], p, unfold_fun args def)], remain) )
    | _ -> raise @@ SyntaxError "let stmt"

let rec parse_type_ands = function
    | (Lex.LIdent name, p) :: (Lex.Eq, _) :: remain ->
        parse_type_body p name [] remain
    | (Lex.TVar targ, p_arg) :: (Lex.LIdent name, p) :: (Lex.Eq, _) :: remain ->
        parse_type_body p name [(targ, p_arg)] remain
    | (Lex.LP, _) :: remain -> (
      match take_targs remain with
      | targs, (Lex.LP, _) :: (Lex.LIdent name, p) :: (Lex.Eq, _) :: remain ->
          parse_type_body p name targs remain
      | _ -> raise @@ SyntaxError "and type targs" )
    | x -> raise @@ SyntaxError (Printf.sprintf "and type %s" @@ show_input_t x)

and parse_type_body p name targs input =
    match parse_ty_variant input with
    | ty, _, (Lex.AndDef, _) :: remain ->
        let ands, remain = parse_type_ands remain in
        ((name, p, targs, ty) :: ands, remain)
    | ty, _, remain -> ([(name, p, targs, ty)], remain)

let parse input =
    match parse_expr input with
    | ast, _, [(Lex.Eof, _)] -> ast
    | x -> dbg x ; raise @@ SyntaxError "top"

(* TODO typeをtype_andsを使って省略 *)
let rec parse_stmts = function
    | (Lex.Type, _) :: (Lex.LIdent name, p) :: (Lex.Eq, _) :: remain ->
        let defs, remain = parse_type_body p name [] remain in
        Type (defs, parse_stmts remain)
    | (Lex.Type, _)
      :: (Lex.TVar tvar, p_arg) :: (Lex.LIdent name, p) :: (Lex.Eq, _) :: remain
      ->
        let defs, remain = parse_type_body p name [(tvar, p_arg)] remain in
        Type (defs, parse_stmts remain)
    | (Lex.Type, _) :: (Lex.LP, _) :: remain -> (
      match take_targs remain with
      | targs, (Lex.RP, _) :: (Lex.LIdent name, p) :: (Lex.Eq, _) :: remain ->
          let defs, remain = parse_type_body p name targs remain in
          Type (defs, parse_stmts remain)
      | _ -> raise @@ SyntaxError "type" )
    | (Lex.Let, _) :: (Lex.Rec, _) :: remain ->
        let defs, remain = parse_letrec_ands remain in
        LetRec (defs, parse_stmts remain)
    | (Lex.Let, _) :: remain ->
        let defs, remain = parse_let_ands remain in
        Let (defs, parse_stmts remain)
    | [(Lex.Eof, _)] -> Never
    | _ -> raise @@ SyntaxError "stmt"

let f fname src = parse_stmts @@ Lex.f fname src
