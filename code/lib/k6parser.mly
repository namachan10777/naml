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
%token Let In
%token Match With
%token Builtin DebugPrint
%token Not

%token Eof

%start main
%start repl
%type <K6ast.stmt_t list> main
%type <K6ast.exp_t> repl

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
    | e = term { K6ast.Cons(e, Emp) }
    | e = term Semicol { K6ast.Cons(e, Emp) }
    | e = term Semicol last = list_inner { K6ast.Cons(e, last) }

fun_arg:
    | True { K6ast.BoolLit true }
    | False { K6ast.BoolLit false }
    | s = Str { K6ast.StrLit s }
    | id = Ident { K6ast.Var id }
    | i = Int { K6ast.IntLit i }
    | LP e = exp RP { e }
    | LB RB { K6ast.Emp }
    | LB inner = list_inner RB { inner }

term:
    | e = fun_arg { e }
    | DebugPrint e = fun_arg { DebugPrint e }
    | Builtin s = Str { K6ast.Builtin s }
    | app = fun_apps { app }
    | Not e = term { K6ast.Not e }
    | Sub e = term %prec Unary { K6ast.Sub(K6ast.IntLit 0, e) }
    | lhr = term Add   rhr = term { K6ast.Add(lhr, rhr) }
    | lhr = term Sub   rhr = term { K6ast.Sub(lhr, rhr) }
    | lhr = term Mul   rhr = term { K6ast.Mul(lhr, rhr) }
    | lhr = term Div   rhr = term { K6ast.Div(lhr, rhr) }
    | lhr = term Mod   rhr = term { K6ast.Mod(lhr, rhr) }
    | lhr = term Gret  rhr = term { K6ast.Gret(lhr, rhr) }
    | lhr = term Less  rhr = term { K6ast.Less(lhr, rhr) }
    | lhr = term Eq    rhr = term { K6ast.Eq(lhr, rhr) }
    | lhr = term Neq   rhr = term { K6ast.Neq(lhr, rhr) }
    | lhr = term And   rhr = term { K6ast.And(lhr, rhr) }
    | lhr = term Or    rhr = term { K6ast.Or(lhr, rhr) }
    | lhr = term Cons  rhr = term { K6ast.Cons(lhr, rhr) }
    | lhr = term Comma rhr = term
        {
            match lhr with
            | K6ast.Tuple t -> K6ast.Tuple (List.append t [rhr])
            | e -> K6ast.Tuple ([e; rhr])
        }

fun_apps:
    | f = fun_arg a = fun_arg 
        { K6ast.App (f, a) }
    | f = fun_apps a = fun_arg
        { K6ast.App (f, a) }

params:
    | arg = Ident { [arg] }
    | arg = Ident ps = params { arg :: ps }

exp_open:
    | Not e = exp_open { K6ast.Not e }
    | Sub e = exp_open { K6ast.Sub(K6ast.IntLit 0, e) }
    | lhr = term Add   rhr = exp_open { K6ast.Add(lhr, rhr) }
    | lhr = term Sub   rhr = exp_open { K6ast.Sub(lhr, rhr) }
    | lhr = term Mul   rhr = exp_open { K6ast.Mul(lhr, rhr) }
    | lhr = term Div   rhr = exp_open { K6ast.Div(lhr, rhr) }
    | lhr = term Mod   rhr = exp_open { K6ast.Mod(lhr, rhr) }
    | lhr = term Gret  rhr = exp_open { K6ast.Gret(lhr, rhr) }
    | lhr = term Less  rhr = exp_open { K6ast.Less(lhr, rhr) }
    | lhr = term Eq    rhr = exp_open { K6ast.Eq(lhr, rhr) }
    | lhr = term Neq   rhr = exp_open { K6ast.Neq(lhr, rhr) }
    | lhr = term And   rhr = exp_open { K6ast.And(lhr, rhr) }
    | lhr = term Or    rhr = exp_open { K6ast.Or(lhr, rhr) }
    | lhr = term Cons  rhr = exp_open { K6ast.Cons(lhr, rhr) }
    | lhr = term Comma rhr = exp_open
        {
            match lhr with
            | Tuple t -> Tuple (List.append t [rhr])
            | e -> Tuple ([e; rhr])
        }
    | Let id = Ident Eq def = exp In expr = exp
        { K6ast.Let (id, def, expr) }
    | Fun ps = params Arrow expr = exp
        { ps |> List.rev |> List.fold_left (fun exp arg -> K6ast.Fun(arg, exp)) expr }
    | Match e = exp With arms=match_arms
        { K6ast.Match (e, arms) }

exp_open_without_match:
    | Not e = exp_open_without_match { K6ast.Not e }
    | Sub e = exp_open_without_match { K6ast.Sub(K6ast.IntLit 0, e) }
    | lhr = term Add   rhr = exp_open_without_match { K6ast.Add(lhr, rhr) }
    | lhr = term Sub   rhr = exp_open_without_match { K6ast.Sub(lhr, rhr) }
    | lhr = term Mul   rhr = exp_open_without_match { K6ast.Mul(lhr, rhr) }
    | lhr = term Div   rhr = exp_open_without_match { K6ast.Div(lhr, rhr) }
    | lhr = term Mod   rhr = exp_open_without_match { K6ast.Mod(lhr, rhr) }
    | lhr = term Gret  rhr = exp_open_without_match { K6ast.Gret(lhr, rhr) }
    | lhr = term Less  rhr = exp_open_without_match { K6ast.Less(lhr, rhr) }
    | lhr = term Eq    rhr = exp_open_without_match { K6ast.Eq(lhr, rhr) }
    | lhr = term Neq   rhr = exp_open_without_match { K6ast.Neq(lhr, rhr) }
    | lhr = term And   rhr = exp_open_without_match { K6ast.And(lhr, rhr) }
    | lhr = term Or    rhr = exp_open_without_match { K6ast.Or(lhr, rhr) }
    | lhr = term Cons  rhr = exp_open_without_match { K6ast.Cons(lhr, rhr) }
    | lhr = term Comma rhr = exp_open_without_match
        {
            match lhr with
            | Tuple t -> Tuple (List.append t [rhr])
            | e -> Tuple ([e; rhr])
        }
    | Let id = Ident Eq def = exp In expr = exp_without_match
        { K6ast.Let (id, def, expr) }
    | Fun ps = params Arrow expr = exp_without_match
        { ps |> List.rev |> List.fold_left (fun exp arg -> K6ast.Fun (arg, exp)) expr }

match_arms:
    | p = pat Arrow e = exp
        { [p, e] }
    | p = pat Arrow e = exp_without_match VBar arms = match_arms
        { (p, e) :: arms }

pat:
    | id = Ident
        { K6ast.PVar id }

seq:
    | lhr = term Semicol rhr = term { K6ast.Seq (lhr, rhr) }
    | lhr = term Semicol rhr = exp_open { K6ast.Seq (lhr, rhr) }
    | lhr = term Semicol rhr = seq { K6ast.Seq (lhr, rhr) }

exp:
    | e = term { e }
    | e = seq { e }
    | e = exp_open { e }

seq_without_match:
    | lhr = term Semicol rhr = term { K6ast.Seq (lhr, rhr) }
    | lhr = term Semicol rhr = exp_open_without_match { K6ast.Seq (lhr, rhr) }
    | lhr = term Semicol rhr = seq_without_match { K6ast.Seq (lhr, rhr) }

exp_without_match:
    | e = term { e }
    | s = seq_without_match { s }
    | e = exp_open_without_match { e }

stmt:
    | Let id = Ident Eq def = exp
        { K6ast.LetStmt (id, def) }

stmts:
    | s = stmt ss = stmts
        { s :: ss }
    |
        { [] }

main:
    ss = stmts Eof { ss }

repl:
    e = exp Eof { e }
