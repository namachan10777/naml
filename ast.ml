type pat_t =
    | PInt of int * Lex.pos_t
    | PBool of bool * Lex.pos_t
    | PVar of Id.t * Lex.pos_t
    | PTuple of pat_t list * Lex.pos_t
    | As of pat_t list * Lex.pos_t
    | Or of pat_t * pat_t list * Lex.pos_t
    | PCtorApp of Id.t * pat_t list * Lex.pos_t
[@@deriving show]

type ty_t =
    | TInt of Lex.pos_t
    | TBool of Lex.pos_t
    | TString of Lex.pos_t
    | TVar of string * Lex.pos_t
    | TTuple of ty_t list * Lex.pos_t
    | TApp of ty_t list * Id.t * Lex.pos_t
[@@deriving show]

type tydef_t = Variant of (Id.t * Lex.pos_t * ty_t list) list | Alias of ty_t
[@@deriving show]

type t =
    | Never
    | Int of int * Lex.pos_t
    | Bool of bool * Lex.pos_t
    | Var of Id.t * Lex.pos_t
    | CtorApp of Id.t * Lex.pos_t * t list
    | Tuple of t list * Lex.pos_t
    | If of t * t * t * Lex.pos_t
    | Let of (pat_t * Lex.pos_t * t) list * t * bool
    | LetRec of (Id.t * Lex.pos_t * t) list * t * bool
    | Fun of Id.t * t * Lex.pos_t
    | Match of t * (pat_t * Lex.pos_t * t * t) list
    | App of t * t * Lex.pos_t
    | Type of (Id.t * Lex.pos_t * (string * Lex.pos_t) list * tydef_t) list * t
[@@deriving show]

let rec of_ty_t = function
    | Parser.TInt p -> TInt p
    | Parser.TBool p -> TBool p
    | Parser.TString p -> TString p
    | Parser.TParen t -> of_ty_t t
    | Parser.TVar (id, p) -> TVar (id, p)
    | Parser.TTuple (ts, p) -> TTuple (List.map of_ty_t ts, p)
    | Parser.TApp (ts, higher, p) -> TApp (List.map of_ty_t ts, higher, p)

let rec of_pat_t = function
    | Parser.PEmp p -> PCtorApp (Id.lookup ["[]"] Pervasives.names, [], p)
    | Parser.PCons (lhr, rhr, p) ->
        PCtorApp
          (Id.lookup ["[]"] Pervasives.names, [of_pat_t lhr; of_pat_t rhr], p)
    | Parser.PInt (i, p) -> PInt (i, p)
    | Parser.PBool (b, p) -> PBool (b, p)
    | Parser.PVar (id, p) -> PVar (id, p)
    | Parser.PTuple (tp, p) -> PTuple (List.map of_pat_t tp, p)
    | Parser.PParen p -> of_pat_t p
    | Parser.PCtor (id, p) -> PCtorApp (id, [], p)
    | Parser.PCtorApp (id, Parser.PParen (Parser.PTuple (ps, _)), p) ->
        PCtorApp (id, List.map of_pat_t ps, p)
    | Parser.PCtorApp (id, pat, p) -> PCtorApp (id, [of_pat_t pat], p)
    | Parser.PAs (pats, p) -> As (List.map of_pat_t pats, p)
    | Parser.POr (pat, pats, p) -> Or (of_pat_t pat, List.map of_pat_t pats, p)

let rec of_t = function
    | Parser.Never -> Never
    | Parser.Int (i, p) -> Int (i, p)
    | Parser.Bool (i, p) -> Bool (i, p)
    | Parser.Var (i, p) -> Var (i, p)
    | Parser.Ctor (i, p) -> CtorApp (i, p, [])
    | Parser.App (Parser.Ctor (n, _), Parser.Tuple (args, _), p) ->
        CtorApp (n, p, List.map of_t args)
    | Parser.App (Parser.Ctor (n, _), t, p) -> CtorApp (n, p, [of_t t])
    | Parser.Emp p -> CtorApp (Id.lookup ["[]"] Pervasives.names, p, [])
    | Parser.Add (lhr, rhr, p) -> op "+" lhr rhr p
    | Parser.Sub (lhr, rhr, p) -> op "-" lhr rhr p
    | Parser.Mul (lhr, rhr, p) -> op "*" lhr rhr p
    | Parser.Div (lhr, rhr, p) -> op "/" lhr rhr p
    | Parser.Mod (lhr, rhr, p) -> op "mod" lhr rhr p
    | Parser.Or (lhr, rhr, p) -> If (of_t lhr, Bool (true, p), of_t rhr, p)
    | Parser.And (lhr, rhr, p) -> If (of_t lhr, of_t rhr, Bool (false, p), p)
    | Parser.Eq (lhr, rhr, p) -> op "=" lhr rhr p
    | Parser.Neq (lhr, rhr, p) -> op "<>" lhr rhr p
    | Parser.Seq (lhr, rhr, p) -> op ";" lhr rhr p
    | Parser.Cons (lhr, rhr, p) ->
        CtorApp (Id.lookup ["::"] Pervasives.names, p, [of_t lhr; of_t rhr])
    | Parser.Gret (lhr, rhr, p) -> op ">" lhr rhr p
    | Parser.Less (lhr, rhr, p) -> op "<" lhr rhr p
    | Parser.Index (lhr, rhr, p) -> op "." lhr rhr p
    | Parser.Neg (e, p) ->
        App (Var (Id.lookup ["<neg>"] Pervasives.names, p), of_t e, p)
    | Parser.Assign (lhr, rhr, p) -> op ":=" lhr rhr p
    | Parser.ArrayAssign (arr, idx, rhr, p) ->
        App
          ( App
              ( App
                  ( Var (Id.lookup ["<arrayassign>"] Pervasives.names, p)
                  , of_t arr
                  , p )
              , of_t idx
              , p )
          , of_t rhr
          , p )
    | Parser.Pipeline (arg, f, p) -> App (of_t f, of_t arg, p)
    | Parser.Tuple (elem, p) -> Tuple (List.map of_t elem, p)
    | Parser.If (cond, e1, e2, p) -> If (of_t cond, of_t e1, of_t e2, p)
    | Parser.Let (defs, expr, is_top) ->
        Let
          ( List.map (fun (pat, p, def) -> (of_pat_t pat, p, of_t def)) defs
          , of_t expr
          , is_top )
    | Parser.LetRec (defs, expr, is_top) ->
        LetRec
          ( List.map (fun (id, p, def) -> (id, p, of_t def)) defs
          , of_t expr
          , is_top )
    | Parser.Fun (params, expr, p) -> Fun (params, of_t expr, p)
    | Parser.App (f, arg, p) -> App (of_t f, of_t arg, p)
    | Parser.Paren e -> of_t e
    | Parser.Match (target, arms) ->
        Match
          ( of_t target
          , List.map
              (fun (pat, p, when_e, expr) ->
                (of_pat_t pat, p, of_t when_e, of_t expr))
              arms )
    | Parser.Type (defs, expr) ->
        Type
          ( List.map
              (function
                | id, p, targs, Parser.Variant pairs ->
                    ( id
                    , p
                    , targs
                    , Variant
                        (List.map
                           (fun (name, p, tys) ->
                             (name, p, List.map of_ty_t tys))
                           pairs) )
                | id, p, targs, Parser.Alias ty ->
                    (id, p, targs, Alias (of_ty_t ty)))
              defs
          , of_t expr )

and op id lhr rhr p =
    App
      (App (Var (Id.lookup [id] Pervasives.names, p), of_t lhr, p), of_t rhr, p)

let f fname src = of_t @@ Parser.f fname src
