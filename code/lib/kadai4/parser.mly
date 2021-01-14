%token <int> Int     (* 0, 1, 2, ... *)
%token <string> Var  (* x, y, hoge, ... *)

%token Plus         (* '+'  *)
%token Minus        (* '-'  *)
%token Asterisk     (* '*'  *)
%token Slash        (* '/'  *)
%token Equal        (* '='  *)
%token NotEq        (* '<>' *)
%token Less         (* '<'  *)
%token Gret         (* '>'  *)
%token ColCol       (* "::" *)

%token LParen       (* '('  *)
%token RParen       (* ')'  *)
%token LBra         (* '['  *)
%token RBra         (* ']'  *)

%token Arrow        (* "->" *)
%token VBar         (* '|'  *)
%token Semicolon    (* ';'  *)

%token True         (* "true"    *)
%token False        (* "false"   *)
%token Fun          (* "fun"     *)
%token Let          (* "let"     *)
%token Rec          (* "rec"     *)
%token In           (* "in"      *)
%token If           (* "if"      *)
%token Then         (* "then"    *)
%token Else         (* "else"    *)
%token Match        (* "match"   *)
%token With         (* "with"    *)
%token Head         (* "List.hd" *) %token Tail         (* "List/tl" *)

%token Eof
%nonassoc With
%left VBar
%left Equal Gret Less NotEq
%right ColCol
%left Plus Minus
%left Asterisk Slash
%nonassoc Unary

%start main
%type <Ast.exp_t> main

%%

main:
    e = exp Eof { e }

list_inner:
    | e = exp { Ast.Cons (e, Ast.Empty) }
    | e = exp Semicolon { Ast.Cons (e, Ast.Empty) }
    | e = exp Semicolon last = list_inner
        { Ast.Cons (e, last) }

arg_exp:
    | sym = Var { Ast.Var sym }
    | lit = Int { Ast.IntLit lit }
    | True { Ast.BoolLit true }
    | False { Ast.BoolLit false }
    | LBra RBra { Ast.Empty }
    | LBra inner = list_inner RBra { inner }
    | LParen e = exp RParen { e }

fun_apps:
    | arg = arg_exp { arg }
    | f = fun_apps arg = arg_exp { Ast.App(f, arg) }

exp_op:
    | apps = fun_apps { apps }
    | Minus e = exp_op %prec Unary
        { Ast.Minus(Ast.IntLit 0, e) }
    | e1 = exp_op Plus e2 = exp_op
        { Ast.Plus(e1, e2) }
    | e1 = exp_op Minus e2 = exp_op
        { Ast.Minus(e1, e2) }
    | e1 = exp_op Asterisk e2 = exp_op
        { Ast.Times(e1, e2) }
    | e1 = exp_op Slash e2 = exp_op
        { Ast.Div(e1, e2) }
    | e1 = exp_op Gret e2 = exp_op
        { Ast.Greater(e1, e2) }
    | e1 = exp_op Less e2 = exp_op
        { Ast.Less(e1, e2) }
    | e1 = exp_op Equal e2 = exp_op
        { Ast.Eq(e1, e2) }
    | e1 = exp_op NotEq e2 = exp_op
        { Ast.Neq(e1, e2) }
    | e1 = exp_op ColCol e2 = exp_op
        { Ast.Cons(e1, e2) }

exp_open:
    | e1 = exp_op Plus e2 = exp_open
        { Ast.Plus(e1, e2) }
    | e1 = exp_op Minus e2 = exp_open
        { Ast.Minus(e1, e2) }
    | e1 = exp_op Asterisk e2 = exp_open
        { Ast.Times(e1, e2) }
    | e1 = exp_op Slash e2 = exp_open
        { Ast.Div(e1, e2) }
    | e1 = exp_op Gret e2 = exp_open
        { Ast.Greater(e1, e2) }
    | e1 = exp_op Less e2 = exp_open
        { Ast.Less(e1, e2) }
    | e1 = exp_op Equal e2 = exp_open
        { Ast.Eq(e1, e2) }
    | e1 = exp_op ColCol e2 = exp_open
        { Ast.Cons(e1, e2) }
    | Tail e = arg_exp
        { Ast.Tail(e) }
    | Head e = arg_exp
        { Ast.Tail(e) }
    | Fun arg = Var Arrow e = exp
        { Ast.Fun(arg, e) }
    | Let var = Var Equal e1 = exp In e2 = exp
        { Ast.Let(var, e1, e2) }
    | Let Rec fname = Var argname = Var Equal e1 = exp In e2 = exp
        { Ast.LetRec(fname, argname, e1, e2) }
    | If cond = exp Then e1 = exp Else e2 = exp
        { Ast.If (cond, e1, e2) }
    | Match e = exp With cases = cases_rev
        { Ast.Match (e, List.rev cases) }

exp:
    | e = exp_op { e }
    | e = exp_open { e }

cases_rev:
    | p = pattern Arrow e = exp
        { [(p, e)] }
    | cases = cases_rev VBar p = pattern Arrow e = exp
        { (p, e) :: cases }
pattern:
    | sym = Var { Ast.Var sym }
    | lit = Int { Ast.IntLit lit }
    | True { Ast.BoolLit true }
    | False { Ast.BoolLit false }
    | LBra RBra { Ast.Empty }
    | p1 = pattern ColCol p2 = pattern { Ast.Cons(p1, p2) }
