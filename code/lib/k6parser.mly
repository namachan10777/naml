(*%token<int> Int*)
%token<string> Ident

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

%%

exp:
    id = Ident { K6ast.Var id }

main:
    e = exp Eof { e }
