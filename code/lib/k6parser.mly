(*%token<int> Int*)
%token<int> Int
%token<string> Ident

%token Add
%token Eq

%token LP
%token RP
%token LB
%token RB

%token Semicol

%token Let
%token In

(*%token Add
%token Sub
%token Mul
%token Div
%token Eq
%token Neq
%token Less
%token Gret
%token Or
%token And
%token Cons
%token LP
%token RP
%token LB
%token RB
%token Arrow
%token VBar
%token Semicol
%token True
%token False
%token Fun
%token Let
%token Rec
%token In
%token If
%token Then
%token Else
%token Match
%token With
%token DebugPrint
%token Builtin*)

%token Eof

%start main
%start repl
%type <K6ast.stmt_t list> main
%type <K6ast.exp_t> repl

%left Add

%%

list_inner:
    | e = term { Cons(e, Emp) }
    | e = term Semicol { Cons(e, Emp) }
    | e = term Semicol last = list_inner { Cons(e, last) }

term:
    | id = Ident { K6ast.Var id }
    | i = Int { K6ast.IntLit i }
    | LP e = exp RP { e }
    | lhr = term Add rhr = term { K6ast.Add(lhr, rhr) }
    | LB RB { K6ast.Emp }
    | LB inner = list_inner RB { inner }

exp_open:
    | lhr = term Add rhr = exp_open { K6ast.Add(lhr, rhr) }
    | Let id = Ident Eq def = exp In expr = exp
        { K6ast.Let (id, def, expr) }

exp:
    | e = term { e }
    | e = exp_open { e }

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
