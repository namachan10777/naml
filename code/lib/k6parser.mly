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
%token Builtin

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
    | e = term { Cons(e, Emp) }
    | e = term Semicol { Cons(e, Emp) }
    | e = term Semicol last = list_inner { Cons(e, last) }

term:
    | s = Str { K6ast.StrLit s }
    | id = Ident { K6ast.Var id }
    | i = Int { K6ast.IntLit i }
    | LP e = exp RP { e }
    | lhr = term Add rhr = term { K6ast.Add(lhr, rhr) }
    | lhr = term Sub rhr = term { K6ast.Sub(lhr, rhr) }
    | lhr = term Mul rhr = term { K6ast.Mul(lhr, rhr) }
    | lhr = term Div rhr = term { K6ast.Div(lhr, rhr) }
    | lhr = term Comma rhr = term
        {
            match lhr with
            | Tuple t -> Tuple (List.append t [rhr])
            | e -> Tuple ([e; rhr])
        }
    | LB RB { K6ast.Emp }
    | LB inner = list_inner RB { inner }
    | Builtin s = Str { K6ast.Builtin s }

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
