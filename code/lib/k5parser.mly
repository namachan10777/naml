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
%type <K5ast.exp_t> main

%%

main:
    e = exp Eof { e }

list_inner:
    | e = exp { K5ast.Cons (e, K5ast.Empty) }
    | e = exp Semicolon { K5ast.Cons (e, K5ast.Empty) }
    | e = exp Semicolon last = list_inner
        { K5ast.Cons (e, last) }

evals:
    | e1 = dont_derive_evals Semicolon e2 = dont_derive_evals { [e1; e2] }
    | e = dont_derive_evals Semicolon last = evals
        { e :: last }
arg_exp:
    | sym = Var { K5ast.Var sym }
    | lit = Int { K5ast.IntLit lit }
    | True { K5ast.BoolLit true }
    | False { K5ast.BoolLit false }
    | LBra RBra { K5ast.Empty }
    | LBra inner = list_inner RBra { inner }
    | LParen e = exp RParen { e }

fun_apps:
    | arg = arg_exp { arg }
    | f = fun_apps arg = arg_exp { K5ast.App(f, arg) }

exp_op:
    | apps = fun_apps { apps }
    | Minus e = exp_op %prec Unary
        { K5ast.Minus(K5ast.IntLit 0, e) }
    | e1 = exp_op Plus e2 = exp_op
        { K5ast.Plus(e1, e2) }
    | e1 = exp_op Minus e2 = exp_op
        { K5ast.Minus(e1, e2) }
    | e1 = exp_op Asterisk e2 = exp_op
        { K5ast.Times(e1, e2) }
    | e1 = exp_op Slash e2 = exp_op
        { K5ast.Div(e1, e2) }
    | e1 = exp_op Gret e2 = exp_op
        { K5ast.Greater(e1, e2) }
    | e1 = exp_op Less e2 = exp_op
        { K5ast.Less(e1, e2) }
    | e1 = exp_op Equal e2 = exp_op
        { K5ast.Eq(e1, e2) }
    | e1 = exp_op NotEq e2 = exp_op
        { K5ast.Neq(e1, e2) }
    | e1 = exp_op ColCol e2 = exp_op
        { K5ast.Cons(e1, e2) }

exp_open:
    | e1 = exp_op Plus e2 = exp_open
        { K5ast.Plus(e1, e2) }
    | e1 = exp_op Minus e2 = exp_open
        { K5ast.Minus(e1, e2) }
    | e1 = exp_op Asterisk e2 = exp_open
        { K5ast.Times(e1, e2) }
    | e1 = exp_op Slash e2 = exp_open
        { K5ast.Div(e1, e2) }
    | e1 = exp_op Gret e2 = exp_open
        { K5ast.Greater(e1, e2) }
    | e1 = exp_op Less e2 = exp_open
        { K5ast.Less(e1, e2) }
    | e1 = exp_op Equal e2 = exp_open
        { K5ast.Eq(e1, e2) }
    | e1 = exp_op ColCol e2 = exp_open
        { K5ast.Cons(e1, e2) }
    | Tail e = arg_exp
        { K5ast.Tail(e) }
    | Head e = arg_exp
        { K5ast.Tail(e) }
    | Fun arg = Var Arrow e = exp
        { K5ast.Fun(arg, e) }
    | Let var = Var Equal e1 = exp In e2 = exp
        { K5ast.Let(var, e1, e2) }
    | Let Rec fname = Var argname = Var Equal e1 = exp In e2 = exp
        { K5ast.LetRec(fname, argname, e1, e2) }
    | If cond = exp Then e1 = exp Else e2 = exp
        { K5ast.If (cond, e1, e2) }
    | Match e = exp With cases = cases_rev
        { K5ast.Match (e, List.rev cases) }

exp:
    | e = exp_op { e }
    | e = exp_open { e }

cases_rev:
    | p = pattern Arrow e = exp
        { [(p, e)] }
    | cases = cases_rev VBar p = pattern Arrow e = exp
        { (p, e) :: cases }
pattern:
    | sym = Var { K5ast.Var sym }
    | lit = Int { K5ast.IntLit lit }
    | True { K5ast.BoolLit true }
    | False { K5ast.BoolLit false }
    | LBra RBra { K5ast.Empty }
    | p1 = pattern ColCol p2 = pattern { K5ast.Cons(p1, p2) }
