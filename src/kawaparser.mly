%{

  open Lexing
  open Kawa

%}

%token PLUS MINUS STAR SLASH
%token LT LE GT GE EQ

%token <int> CST
%token <bool> BOOL
%token <string> IDENT
%token TYP_VOID TYP_INT TYP_BOOL
%token NEW CLASS EXTENDS METHOD DOT MAIN THIS VAR ATTRIBUTE
%token LPAR RPAR COMMA BEGIN END SEMI
%token PUTCHAR SET IF ELSE WHILE RETURN
%token EOF

%nonassoc LT LE GT GE EQ
%left PLUS MINUS
%left STAR SLASH
%left DOT

%start program
%type <Kawa.program> program

%%

program:
| globals=list(variable_decl) classes=list(class_def)
    MAIN BEGIN main=list(instruction) END EOF
    { {classes; globals; main} }
| error { let pos = $startpos in
          let message =
            Printf.sprintf
              "Syntax error at %d, %d"
              pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
          in
          failwith message }
;

attribute_decl:
| ATTRIBUTE tid=typed_ident SEMI { tid }
;
  
variable_decl:
| VAR tid=typed_ident SEMI { tid }
;

typed_ident:
| t=typ id=IDENT { id, t }
;
  
typ:
| TYP_VOID { Typ_Void }
| TYP_INT { Typ_Int }
| TYP_BOOL { Typ_Bool }
| c=IDENT { Typ_Class c }
;

extension:
| EXTENDS class_name=IDENT { class_name }
;
  
class_def:
| CLASS class_name=IDENT parent=option(extension)
    BEGIN attributes=list(attribute_decl) methods=list(method_def) END
    { {class_name; attributes; methods; parent} }
;
    
method_def:
| METHOD return=typ method_name=IDENT LPAR params=separated_list(COMMA, typed_ident) RPAR
    BEGIN locals=list(variable_decl) code=list(instruction) END
    { {method_name; code; params; locals; return} }
;

mem_access:
| x=IDENT { Var x }
| e=expression DOT field=IDENT { Field(e, field) }
;
  
instruction:
| PUTCHAR LPAR e=expression RPAR SEMI { Putchar(e) }
| a=mem_access SET e=expression SEMI { Set(a, e) }
| IF LPAR c=expression RPAR
    BEGIN s1=list(instruction) END
    ELSE BEGIN s2=list(instruction) END { If(c, s1, s2) }
| WHILE LPAR c=expression RPAR
    BEGIN s=list(instruction) END { While(c, s) }
| RETURN e=expression SEMI { Return(e) }
| e=expression SEMI { Expr(e) }
;

expression:
| n=CST { Cst(n) }
| MINUS n=CST { Cst(~-n) }
| b=BOOL { Bool(b) }
| a=mem_access { Get(a) }
| LPAR e=expression RPAR { e }
| e1=expression op=binop e2=expression { Binop(op, e1, e2) }
| THIS { This }
| NEW class_name=IDENT LPAR params=separated_list(COMMA, expression) RPAR
                             { New(class_name, params) }
| e=expression DOT m=IDENT LPAR params=separated_list(COMMA, expression) RPAR
                             { MethCall(e, m, params) }
;

%inline binop:
| MINUS { Sub }
| PLUS { Add }
| STAR { Mul }
| SLASH { Div }
| LT { Lt }
| LE { Le }
| GT { Gt }
| GE { Ge }
| EQ { Eq }
;

