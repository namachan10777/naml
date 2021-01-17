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
%type <K4ast.exp_t> main

%%

main:
    e = exp Eof { e }

list_inner:
    | e = exp { K4ast.Cons (e, K4ast.Empty) }
    | e = exp Semicolon { K4ast.Cons (e, K4ast.Empty) }
    | e = exp Semicolon last = list_inner
        { K4ast.Cons (e, last) }

arg_exp:
    | sym = Var { K4ast.Var sym }
    | lit = Int { K4ast.IntLit lit }
    | True { K4ast.BoolLit true }
    | False { K4ast.BoolLit false }
    | LBra RBra { K4ast.Empty }
    | LBra inner = list_inner RBra { inner }
    | LParen e = exp RParen { e }

fun_apps:
    | arg = arg_exp { arg }
    | f = fun_apps arg = arg_exp { K4ast.App(f, arg) }

exp_op:
    | apps = fun_apps { apps }
    | Minus e = exp_op %prec Unary
        { K4ast.Minus(K4ast.IntLit 0, e) }
    | e1 = exp_op Plus e2 = exp_op
        { K4ast.Plus(e1, e2) }
    | e1 = exp_op Minus e2 = exp_op
        { K4ast.Minus(e1, e2) }
    | e1 = exp_op Asterisk e2 = exp_op
        { K4ast.Times(e1, e2) }
    | e1 = exp_op Slash e2 = exp_op
        { K4ast.Div(e1, e2) }
    | e1 = exp_op Gret e2 = exp_op
        { K4ast.Greater(e1, e2) }
    | e1 = exp_op Less e2 = exp_op
        { K4ast.Less(e1, e2) }
    | e1 = exp_op Equal e2 = exp_op
        { K4ast.Eq(e1, e2) }
    | e1 = exp_op NotEq e2 = exp_op
        { K4ast.Neq(e1, e2) }
    | e1 = exp_op ColCol e2 = exp_op
        { K4ast.Cons(e1, e2) }

exp_open:
    | e1 = exp_op Plus e2 = exp_open
        { K4ast.Plus(e1, e2) }
    | e1 = exp_op Minus e2 = exp_open
        { K4ast.Minus(e1, e2) }
    | e1 = exp_op Asterisk e2 = exp_open
        { K4ast.Times(e1, e2) }
    | e1 = exp_op Slash e2 = exp_open
        { K4ast.Div(e1, e2) }
    | e1 = exp_op Gret e2 = exp_open
        { K4ast.Greater(e1, e2) }
    | e1 = exp_op Less e2 = exp_open
        { K4ast.Less(e1, e2) }
    | e1 = exp_op Equal e2 = exp_open
        { K4ast.Eq(e1, e2) }
    | e1 = exp_op ColCol e2 = exp_open
        { K4ast.Cons(e1, e2) }
    | Tail e = arg_exp
        { K4ast.Tail(e) }
    | Head e = arg_exp
        { K4ast.Tail(e) }
    | Fun arg = Var Arrow e = exp
        { K4ast.Fun(arg, e) }
    | Let var = Var Equal e1 = exp In e2 = exp
        { K4ast.Let(var, e1, e2) }
    | Let Rec fname = Var argname = Var Equal e1 = exp In e2 = exp
        { K4ast.LetRec(fname, argname, e1, e2) }
    | If cond = exp Then e1 = exp Else e2 = exp
        { K4ast.If (cond, e1, e2) }
    | Match e = exp With cases = cases_rev
        { K4ast.Match (e, List.rev cases) }

exp:
    | e = exp_op { e }
    | e = exp_open { e }

cases_rev:
    | p = pattern Arrow e = exp
        { [(p, e)] }
    | cases = cases_rev VBar p = pattern Arrow e = exp
        { (p, e) :: cases }
pattern:
    | sym = Var { K4ast.Var sym }
    | lit = Int { K4ast.IntLit lit }
    | True { K4ast.BoolLit true }
    | False { K4ast.BoolLit false }
    | LBra RBra { K4ast.Empty }
    | p1 = pattern ColCol p2 = pattern { K4ast.Cons(p1, p2) }
