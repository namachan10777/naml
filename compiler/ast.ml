type id_t = string list [@@deriving show]

type pat_t =
    | PEmp
    | PCons of pat_t * pat_t
    | PInt of int
    | PBool of bool
    | PVar of string
    | PTuple of pat_t list
    | As of pat_t list
    | Or of pat_t * pat_t list
    | PCtorApp of string list * pat_t list
    | PCtor of string list
[@@deriving show]

type ty_t =
    | TInt
    | TBool
    | TString
    | TVar of string
    | TTuple of ty_t list
    | TApp of ty_t list * string list
[@@deriving show]

type tydef_t = Variant of (string * ty_t list) list | Alias of ty_t
[@@deriving show]

type t =
    | Never
    | Int of int
    | Bool of bool
    | Var of string list
    | Ctor of string list
    | CtorApp of string list * t list
    | Tuple of t list
    | If of t * t * t
    | Let of (pat_t * t) list * t
    | LetRec of (string list * t) list * t
    | Fun of string list * t
    | Match of t * (pat_t * t * t) list
    | App of t * t list
    | Type of (string * string list * tydef_t) list * t
[@@deriving show]

let rec of_parser_ty_t = function
    | Parser.TInt -> TInt
    | Parser.TBool -> TBool
    | Parser.TString -> TString
    | Parser.TParen t -> of_parser_ty_t t
    | Parser.TVar id -> TVar id
    | Parser.TTuple ts -> TTuple (List.map of_parser_ty_t ts)
    | Parser.TApp (ts, higher) -> TApp (List.map of_parser_ty_t ts, higher)

let rec of_parser_pat_t = function
    | Parser.PEmp -> PEmp
    | Parser.PCons (lhr, rhr) -> PCons (of_parser_pat_t lhr, of_parser_pat_t rhr)
    | Parser.PInt i -> PInt i
    | Parser.PBool b -> PBool b
    | Parser.PVar id -> PVar id
    | Parser.PTuple tp -> PTuple (List.map of_parser_pat_t tp)
    | Parser.PParen p -> of_parser_pat_t p
    | Parser.PCtor id -> PCtor id
    | Parser.PCtorApp (id, Parser.PParen (Parser.PTuple ps)) ->
        PCtorApp (id, List.map of_parser_pat_t ps)
    | Parser.PCtorApp (id, p) -> PCtorApp (id, [of_parser_pat_t p])
    | Parser.PAs ps -> As (List.map of_parser_pat_t ps)
    | Parser.POr (p, ps) -> Or (of_parser_pat_t p, List.map of_parser_pat_t ps)

let rec of_parser_t = function
    | Parser.Never -> Never
    | Parser.Int i -> Int i
    | Parser.Bool i -> Bool i
    | Parser.Var i -> Var i
    | Parser.Ctor i -> Ctor i
    | Parser.App (Parser.Ctor n, [Parser.Tuple args]) ->
        CtorApp (n, List.map of_parser_t args)
    | Parser.App (Parser.Ctor n, [t]) -> CtorApp (n, [of_parser_t t])
    | Parser.Emp -> Ctor ["[]"]
    | Parser.Add (lhr, rhr) -> op "+" lhr rhr
    | Parser.Sub (lhr, rhr) -> op "-" lhr rhr
    | Parser.Mul (lhr, rhr) -> op "*" lhr rhr
    | Parser.Div (lhr, rhr) -> op "/" lhr rhr
    | Parser.Mod (lhr, rhr) -> op "mod" lhr rhr
    | Parser.Or (lhr, rhr) -> If (of_parser_t lhr, Bool true, of_parser_t rhr)
    | Parser.And (lhr, rhr) -> If (of_parser_t lhr, of_parser_t rhr, Bool false)
    | Parser.Eq (lhr, rhr) -> op "=" lhr rhr
    | Parser.Neq (lhr, rhr) -> op "<>" lhr rhr
    | Parser.Seq (lhr, rhr) -> op ";" lhr rhr
    | Parser.Cons (lhr, rhr) ->
        CtorApp (["::"], [of_parser_t lhr; of_parser_t rhr])
    | Parser.Gret (lhr, rhr) -> op ">" lhr rhr
    | Parser.Less (lhr, rhr) -> op "<" lhr rhr
    | Parser.Index (lhr, rhr) -> op "." lhr rhr
    | Parser.Neg e -> App (Var ["<neg>"], [of_parser_t e])
    | Parser.Assign (lhr, rhr) -> op ":=" lhr rhr
    | Parser.ArrayAssign (arr, idx, rhr) ->
        App
          ( Var ["<arrayassign>"]
          , [of_parser_t arr; of_parser_t idx; of_parser_t rhr] )
    | Parser.Pipeline (arg, f) -> App (of_parser_t f, [of_parser_t arg])
    | Parser.Tuple elem -> Tuple (List.map of_parser_t elem)
    | Parser.If (cond, e1, e2) ->
        If (of_parser_t cond, of_parser_t e1, of_parser_t e2)
    | Parser.Let (defs, expr) ->
        Let
          ( List.map
              (fun (pat, def) -> (of_parser_pat_t pat, of_parser_t def))
              defs
          , of_parser_t expr )
    | Parser.LetRec (defs, expr) ->
        LetRec
          ( List.map (fun (id, def) -> (id, of_parser_t def)) defs
          , of_parser_t expr )
    | Parser.Fun (params, expr) -> Fun (params, of_parser_t expr)
    | Parser.App (f, arg) -> App (of_parser_t f, List.map of_parser_t arg)
    | Parser.Paren e -> of_parser_t e
    | Parser.Match (target, arms) ->
        Match
          ( of_parser_t target
          , List.map
              (fun (pat, when_e, expr) ->
                (of_parser_pat_t pat, of_parser_t when_e, of_parser_t expr))
              arms )
    | Parser.Type (defs, expr) ->
        Type
          ( List.map
              (function
                | id, targs, Parser.Variant pairs ->
                    ( id
                    , targs
                    , Variant
                        (List.map
                           (fun (name, tys) ->
                             (name, List.map of_parser_ty_t tys))
                           pairs) )
                | id, targs, Parser.Alias ty ->
                    (id, targs, Alias (of_parser_ty_t ty)))
              defs
          , of_parser_t expr )

and op id lhr rhr = App (Var [id], [of_parser_t lhr; of_parser_t rhr])

let f fname src = of_parser_t @@ Parser.f fname src
