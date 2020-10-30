%token <int> Int
%token LParen
%token RParen
%token Eof

%start main
%type <Ast.exp_t> main

%%

exp:
| lit = Int { Ast.IntLit lit }
| LParen e = exp RParen { e }

main:
    e = exp Eof { e }
