%token <int> Int     (* 0, 1, 2, ... *)
%token <string> Var  (* x, y, hoge, ... *)

%token Plus         (* '+'  *)
%token Minus        (* '-'  *)
%token Asterisk     (* '*'  *)
%token Slash        (* '/'  *)
%token Equal        (* '='  *)
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
%token Head         (* "List.hd" *)
%token Tail         (* "List/tl" *)

%token Eof
%nonassoc Else Arrow With If Let
%left VBar
%left Equal Gret Less
%right ColCol
%left Plus Minus
%left Asterisk Slash
%nonassoc Unary
%left Var Int True False LBra LParen

%start main
%type <Kadai4_ast.exp_t> main

%%

main:
    e = exp Eof { e }

list_inner:
    | e = exp { Kadai4_ast.Cons (e, Kadai4_ast.Empty) }
    | e = exp Semicolon { Kadai4_ast.Cons (e, Kadai4_ast.Empty) }
    | e = exp Semicolon last = list_inner
        { Kadai4_ast.Cons (e, last) }

arg_exp:
    | sym = Var { Kadai4_ast.Var sym }
    | lit = Int { Kadai4_ast.IntLit lit }
    | True { Kadai4_ast.BoolLit true }
    | False { Kadai4_ast.BoolLit false }
    | LBra RBra { Kadai4_ast.Empty }
    | LBra inner = list_inner RBra { inner }
    | LParen e = exp RParen { e }

exp_fun:
    | LParen e = exp RParen { e }
    | sym = Var { Kadai4_ast.Var sym }

exp_op:
    | e = arg_exp { e }
    | Minus e = exp_op %prec Unary
        { Kadai4_ast.Minus(Kadai4_ast.IntLit 0, e) }
    | e1 = exp_op Plus e2 = exp_op
        { Kadai4_ast.Plus(e1, e2) }
    | e1 = exp_op Minus e2 = exp_op
        { Kadai4_ast.Minus(e1, e2) }
    | e1 = exp_op Asterisk e2 = exp_op
        { Kadai4_ast.Times(e1, e2) }
    | e1 = exp_op Slash e2 = exp_op
        { Kadai4_ast.Div(e1, e2) }
    | e1 = exp_op Gret e2 = exp_op
        { Kadai4_ast.Greater(e1, e2) }
    | e1 = exp_op Less e2 = exp_op
        { Kadai4_ast.Less(e1, e2) }
    | e1 = exp_op Equal e2 = exp_op
        { Kadai4_ast.Eq(e1, e2) }
    | e1 = exp_op ColCol e2 = exp_op
        { Kadai4_ast.Cons(e1, e2) }

exp_open:
    | e1 = exp_op Plus e2 = exp_open
        { Kadai4_ast.Plus(e1, e2) }
    | e1 = exp_op Minus e2 = exp_open
        { Kadai4_ast.Minus(e1, e2) }
    | e1 = exp_op Asterisk e2 = exp_open
        { Kadai4_ast.Times(e1, e2) }
    | e1 = exp_op Slash e2 = exp_open
        { Kadai4_ast.Div(e1, e2) }
    | e1 = exp_op Gret e2 = exp_open
        { Kadai4_ast.Greater(e1, e2) }
    | e1 = exp_op Less e2 = exp_open
        { Kadai4_ast.Less(e1, e2) }
    | e1 = exp_op Equal e2 = exp_open
        { Kadai4_ast.Eq(e1, e2) }
    | e1 = exp_op ColCol e2 = exp_open
        { Kadai4_ast.Cons(e1, e2) }
    | f = exp_fun arg = arg_exp { Kadai4_ast.App(f, arg) }
    | Tail e = arg_exp
        { Kadai4_ast.Tail(e) }
    | Head e = arg_exp
        { Kadai4_ast.Tail(e) }
    | Fun arg = Var Arrow e = exp
        { Kadai4_ast.Fun(arg, e) }
    | Let var = Var Equal e1 = exp In e2 = exp
        { Kadai4_ast.Let(var, e1, e2) }
    | Let Rec fname = Var argname = Var Equal e1 = exp In e2 = exp
        { Kadai4_ast.LetRec(fname, argname, e1, e2) }
    | If cond = exp Then e1 = exp Else e2 = exp
        { Kadai4_ast.If (cond, e1, e2) }
    | Match e = exp With cases = cases_rev
        { Kadai4_ast.Match (e, List.rev cases) }

exp:
    | e = exp_op { e }
    | e = exp_open { e }

cases_rev:
    | p = pattern Arrow e = exp
        { [(p, e)] }
    | cases = cases_rev VBar p = pattern Arrow e = exp
        { (p, e) :: cases }
pattern:
    | sym = Var { Kadai4_ast.Var sym }
    | lit = Int { Kadai4_ast.IntLit lit }
    | True { Kadai4_ast.BoolLit true }
    | False { Kadai4_ast.BoolLit false }
    | LBra RBra { Kadai4_ast.Empty }
    | p1 = pattern ColCol p2 = pattern { Kadai4_ast.Cons(p1, p2) }
