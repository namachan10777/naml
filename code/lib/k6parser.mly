(*%token<int> Int*)
%token<int> Int
%token<string> Ident
%token<string> Str

%token Add Sub Mul Div
%token Eq
%token LP RP LB RB
%token Semicol Comma VBar Arrow
%token Let In
%token Match With
%token Builtin DebugPrint

%token Eof

%start main
%start repl
%type <K6ast.stmt_t list> main
%type <K6ast.exp_t> repl

%left Comma
%left Add Sub
%left Mul Div

%%

list_inner:
    | e = term { K6ast.Cons(e, Emp) }
    | e = term Semicol { K6ast.Cons(e, Emp) }
    | e = term Semicol last = list_inner { K6ast.Cons(e, last) }

fun_arg:
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
    | lhr = term Add rhr = term { K6ast.Add(lhr, rhr) }
    | lhr = term Sub rhr = term { K6ast.Sub(lhr, rhr) }
    | lhr = term Mul rhr = term { K6ast.Mul(lhr, rhr) }
    | lhr = term Div rhr = term { K6ast.Div(lhr, rhr) }
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

exp_open:
    | lhr = term Add rhr = exp_open { K6ast.Add(lhr, rhr) }
    | lhr = term Sub rhr = exp_open { K6ast.Sub(lhr, rhr) }
    | lhr = term Mul rhr = exp_open { K6ast.Mul(lhr, rhr) }
    | lhr = term Div rhr = exp_open { K6ast.Div(lhr, rhr) }
    | lhr = term Comma rhr = exp_open
        {
            match lhr with
            | Tuple t -> Tuple (List.append t [rhr])
            | e -> Tuple ([e; rhr])
        }
    | Let id = Ident Eq def = exp In expr = exp
        { K6ast.Let (id, def, expr) }
    | Match e = exp With arms=match_arms
        { K6ast.Match (e, arms) }

exp_open_without_match:
    | lhr = term Add rhr = exp_open_without_match { K6ast.Add(lhr, rhr) }
    | lhr = term Sub rhr = exp_open_without_match { K6ast.Sub(lhr, rhr) }
    | lhr = term Mul rhr = exp_open_without_match { K6ast.Mul(lhr, rhr) }
    | lhr = term Div rhr = exp_open_without_match { K6ast.Div(lhr, rhr) }
    | lhr = term Comma rhr = exp_open_without_match
        {
            match lhr with
            | Tuple t -> Tuple (List.append t [rhr])
            | e -> Tuple ([e; rhr])
        }
    | Let id = Ident Eq def = exp In expr = exp_without_match
        { K6ast.Let (id, def, expr) }

match_arms:
    | p = pat Arrow e = exp
        { [p, e] }
    | p = pat Arrow e = exp_without_match VBar arms = match_arms
        { (p, e) :: arms }

pat:
    | id = Ident
        { K6ast.PVar id }

exp:
    | e = term { e }
    | e = exp_open { e }

exp_without_match:
    | e = term { e }
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
