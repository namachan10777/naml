(*%token<int> Int*)
%token<int> Int
%token<string> Ident
%token<string> Str
%token True False

%token Add Sub Mul Div Mod
%token Cons
%token Gret Less
%token Eq Neq And Or
%token LP RP LB RB
%token Semicol Comma VBar Arrow
%token Fun
%token If Then Else
%token Let In Rec
%token Match With
%token Builtin DebugPrint
%token Not

%token Eof

%start main
%start repl
%type <K7ast.stmt_t list> main
%type <K7ast.exp_t> repl

%left Comma
%left Or
%left And
%nonassoc Eq Neq Gret Less
%left Add Sub
%left Mul Div Mod
%right Cons
%nonassoc Unary Not

%%

list_inner:
    | e = term { K7ast.Cons(e, Emp) }
    | e = term Semicol { K7ast.Cons(e, Emp) }
    | e = term Semicol last = list_inner { K7ast.Cons(e, last) }

fun_arg:
    | True { K7ast.BoolLit true }
    | False { K7ast.BoolLit false }
    | s = Str { K7ast.StrLit s }
    | id = Ident { K7ast.Var id }
    | i = Int { K7ast.IntLit i }
    | LP e = exp RP { e }
    | LB RB { K7ast.Emp }
    | LB inner = list_inner RB { inner }

term:
    | e = fun_arg { e }
    | DebugPrint e = fun_arg { DebugPrint e }
    | Builtin s = Str { K7ast.Builtin s }
    | app = fun_apps { app }
    | Not e = term { K7ast.Not e }
    | Sub e = term %prec Unary { K7ast.Sub(K7ast.IntLit 0, e) }
    | lhr = term Add   rhr = term { K7ast.Add(lhr, rhr) }
    | lhr = term Sub   rhr = term { K7ast.Sub(lhr, rhr) }
    | lhr = term Mul   rhr = term { K7ast.Mul(lhr, rhr) }
    | lhr = term Div   rhr = term { K7ast.Div(lhr, rhr) }
    | lhr = term Mod   rhr = term { K7ast.Mod(lhr, rhr) }
    | lhr = term Gret  rhr = term { K7ast.Gret(lhr, rhr) }
    | lhr = term Less  rhr = term { K7ast.Less(lhr, rhr) }
    | lhr = term Eq    rhr = term { K7ast.Eq(lhr, rhr) }
    | lhr = term Neq   rhr = term { K7ast.Neq(lhr, rhr) }
    | lhr = term And   rhr = term { K7ast.And(lhr, rhr) }
    | lhr = term Or    rhr = term { K7ast.Or(lhr, rhr) }
    | lhr = term Cons  rhr = term { K7ast.Cons(lhr, rhr) }
    | lhr = term Comma rhr = term
        {
            match lhr with
            | K7ast.Tuple t -> K7ast.Tuple (List.append t [rhr])
            | e -> K7ast.Tuple ([e; rhr])
        }

fun_apps:
    | f = fun_arg a = fun_arg 
        { K7ast.App (f, a) }
    | f = fun_apps a = fun_arg
        { K7ast.App (f, a) }

params:
    | arg = Ident { [arg] }
    | arg = Ident ps = params { arg :: ps }

exp_open:
    | Not e = exp_open { K7ast.Not e }
    | Sub e = exp_open { K7ast.Sub(K7ast.IntLit 0, e) }
    | lhr = term Add   rhr = exp_open { K7ast.Add(lhr, rhr) }
    | lhr = term Sub   rhr = exp_open { K7ast.Sub(lhr, rhr) }
    | lhr = term Mul   rhr = exp_open { K7ast.Mul(lhr, rhr) }
    | lhr = term Div   rhr = exp_open { K7ast.Div(lhr, rhr) }
    | lhr = term Mod   rhr = exp_open { K7ast.Mod(lhr, rhr) }
    | lhr = term Gret  rhr = exp_open { K7ast.Gret(lhr, rhr) }
    | lhr = term Less  rhr = exp_open { K7ast.Less(lhr, rhr) }
    | lhr = term Eq    rhr = exp_open { K7ast.Eq(lhr, rhr) }
    | lhr = term Neq   rhr = exp_open { K7ast.Neq(lhr, rhr) }
    | lhr = term And   rhr = exp_open { K7ast.And(lhr, rhr) }
    | lhr = term Or    rhr = exp_open { K7ast.Or(lhr, rhr) }
    | lhr = term Cons  rhr = exp_open { K7ast.Cons(lhr, rhr) }
    | lhr = term Comma rhr = exp_open
        {
            match lhr with
            | Tuple t -> Tuple (List.append t [rhr])
            | e -> Tuple ([e; rhr])
        }
    | Let id = Ident Eq def = exp In expr = exp
        { K7ast.Let (id, def, expr) }
    | Let id = Ident ps = params Eq def = exp In expr = exp
        { K7ast.Let (id, ps |> List.rev |> List.fold_left (fun exp arg -> K7ast.Fun(arg, exp)) def, expr) }
    | Let Rec id = Ident Eq def = exp In expr = exp
        { K7ast.LetRec (id, def, expr) }
    | Let Rec id = Ident ps = params Eq def = exp In expr = exp
        { K7ast.LetRec (id, ps |> List.rev |> List.fold_left (fun exp arg -> K7ast.Fun(arg, exp)) def, expr) }
    | If cond = exp Then exp_then = exp Else exp_else = exp
        { K7ast.If (cond, exp_then, exp_else) }
    | Fun ps = params Arrow expr = exp
        { ps |> List.rev |> List.fold_left (fun exp arg -> K7ast.Fun(arg, exp)) expr }
    | Match e = exp With arms=match_arms
        { K7ast.Match (e, arms) }

exp_open_without_match:
    | Not e = exp_open_without_match { K7ast.Not e }
    | Sub e = exp_open_without_match { K7ast.Sub(K7ast.IntLit 0, e) }
    | lhr = term Add   rhr = exp_open_without_match { K7ast.Add(lhr, rhr) }
    | lhr = term Sub   rhr = exp_open_without_match { K7ast.Sub(lhr, rhr) }
    | lhr = term Mul   rhr = exp_open_without_match { K7ast.Mul(lhr, rhr) }
    | lhr = term Div   rhr = exp_open_without_match { K7ast.Div(lhr, rhr) }
    | lhr = term Mod   rhr = exp_open_without_match { K7ast.Mod(lhr, rhr) }
    | lhr = term Gret  rhr = exp_open_without_match { K7ast.Gret(lhr, rhr) }
    | lhr = term Less  rhr = exp_open_without_match { K7ast.Less(lhr, rhr) }
    | lhr = term Eq    rhr = exp_open_without_match { K7ast.Eq(lhr, rhr) }
    | lhr = term Neq   rhr = exp_open_without_match { K7ast.Neq(lhr, rhr) }
    | lhr = term And   rhr = exp_open_without_match { K7ast.And(lhr, rhr) }
    | lhr = term Or    rhr = exp_open_without_match { K7ast.Or(lhr, rhr) }
    | lhr = term Cons  rhr = exp_open_without_match { K7ast.Cons(lhr, rhr) }
    | lhr = term Comma rhr = exp_open_without_match
        {
            match lhr with
            | Tuple t -> Tuple (List.append t [rhr])
            | e -> Tuple ([e; rhr])
        }
    | Let id = Ident Eq def = exp In expr = exp_without_match
        { K7ast.Let (id, def, expr) }
    | Let id = Ident ps = params Eq def = exp In expr = exp_without_match
        { K7ast.Let (id, ps |> List.rev |> List.fold_left (fun exp arg -> K7ast.Fun(arg, exp)) def, expr) }
    | Let Rec id = Ident Eq def = exp In expr = exp_without_match
        { K7ast.LetRec (id, def, expr) }
    | Let Rec id = Ident ps = params Eq def = exp In expr = exp_without_match
        { K7ast.LetRec (id, ps |> List.rev |> List.fold_left (fun exp arg -> K7ast.Fun(arg, exp)) def, expr) }
    | If cond = exp Then exp_then = exp Else exp_else = exp_without_match
        { K7ast.If (cond, exp_then, exp_else) }
    | Fun ps = params Arrow expr = exp_without_match
        { ps |> List.rev |> List.fold_left (fun exp arg -> K7ast.Fun (arg, exp)) expr }

match_arms:
    | p = pat Arrow e = exp
        { [p, e] }
    | p = pat Arrow e = exp_without_match VBar arms = match_arms
        { (p, e) :: arms }

seq:
    | lhr = term Semicol rhr = term { K7ast.Seq (lhr, rhr) }
    | lhr = term Semicol rhr = exp_open { K7ast.Seq (lhr, rhr) }
    | lhr = term Semicol rhr = seq { K7ast.Seq (lhr, rhr) }

exp:
    | e = term { e }
    | e = seq { e }
    | e = exp_open { e }

seq_without_match:
    | lhr = term Semicol rhr = term { K7ast.Seq (lhr, rhr) }
    | lhr = term Semicol rhr = exp_open_without_match { K7ast.Seq (lhr, rhr) }
    | lhr = term Semicol rhr = seq_without_match { K7ast.Seq (lhr, rhr) }

exp_without_match:
    | e = term { e }
    | s = seq_without_match { s }
    | e = exp_open_without_match { e }

stmt:
    | Let id = Ident Eq def = exp
        { K7ast.LetStmt (id, def) }

stmts:
    | s = stmt ss = stmts
        { s :: ss }
    |
        { [] }

plist_inner:
    | e = pterm { K7ast.PCons(e, PEmp) }
    | e = pterm Semicol { K7ast.PCons(e, PEmp) }
    | e = pterm Semicol last = plist_inner { K7ast.PCons(e, last) }

pterm:
    | i = Int  { K7ast.PIntLit i }
    | True  { K7ast.PBoolLit true }
    | False  { K7ast.PBoolLit false }
    | id = Ident { K7ast.PVar id }
    | LB RB { K7ast.PEmp }
    | lhr = pterm Cons rhr = pterm { K7ast.PCons (lhr, rhr) }
    | LB inner = plist_inner RB { inner }
    | LP p = pat RP { p }

ptuple:
    | lhr = pterm Comma rhr = pterm { [lhr; rhr] }
    | lhr = pterm Comma rhr = ptuple { lhr :: rhr }

pat:
    | tp = ptuple { K7ast.PTuple tp }
    | t = pterm { t }

main:
    ss = stmts Eof { ss }

repl:
    e = exp Eof { e }
