(*%token<int> Int*)
%token<int> Int
%token<string> Ident

%token Add
%token LP
%token RP
%token Let
%token Eq
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
%type <K6ast.exp_t> main

%left Add

%%

term:
    | id = Ident { K6ast.Var id }
    | i = Int { K6ast.IntLit i }
    | LP e = exp RP { e }
    | lhr = term Add rhr = term { K6ast.Add(lhr, rhr) }

exp_open:
    | lhr = term Add rhr = exp_open { K6ast.Add(lhr, rhr) }
    | Let id = Ident Eq def = exp In expr = exp
        { K6ast.Let (id, def, expr) }

exp:
    | e = term { e }
    | e = exp_open { e }

main:
    e = exp Eof { e }
