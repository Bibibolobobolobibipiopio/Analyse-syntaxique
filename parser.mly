%{
open ExpDessin
%}

%token<string> HEXCOULEUR
(*etape 1*)
%token EOF AVANCER TOURNER BAISSERPIN LEVERPIN LPAR RPAR FOIS DIV PLUS MOINS MOINSEXP POINTVIRG VAR
(*etape 2*)
%token<int> NOMBRE 
%token COULEURPINCEAU LARGEURPINCEAU NOIR BLEU ROUGE VERT JAUNE
(*etape 3*)
%token EGAL

(*etape 4*)
%token WHILE FAIRE FOR DEFOIS DEBUT FIN IF THEN ELSE INF SUP EGALITE NOTEGAL INFOREG SUPOREG AND OR NOT TRUE FALSE
(*%token VARIDDECL VARIDINIT IDDECL EGAL*)
%start<ExpDessin.blocInst> input
%token<string> ID


%nonassoc BAISSERPIN LEVERPIN
%nonassoc THEN
%nonassoc ELSE
%left PLUS MOINS
%left FOIS DIV
%left LPAR RPAR
%left TOURNER AVANCER
%right MOINSEXP
%left OR
%left AND
%right NOT




%%

  
input:
  |c=action POINTVIRG recIn = input { Instruction(c, recIn) } 
  | EOF {EndOfFile}
  | {Fin}


action:
    LEVERPIN {LeverLePinceau}
  | BAISSERPIN {BaisserLePinceau}
  | AVANCER e=expression {Avancer(e)}
  | TOURNER e=expression {Tourner(e)}
  | COULEURPINCEAU c=couleur {CouleurPinceau(c)}
  | LARGEURPINCEAU e=expression {LargeurPinceau(e)}
  (*il faudra checker qu'on l'a bien recup mais jpp le faire maintenant*)
  | VAR s=ID EGAL e=expression {Variable(VarDecl(s, e))}
  | s=ID EGAL e=expression {Variable(VarInit(s, e))}
  | VAR s=ID {Variable(Var(s))}
  | DEBUT b=blocInst FIN { Bloc(b) }
  | WHILE c=condition FAIRE a=action  {Boucle(TantQue(c, a))}
  | FOR e=expression DEFOIS  a=action  {Boucle(For(e, a))}
  | IF c=condition THEN i=action ELSE i2=action { BlocCondition(IfThenElse(c, i, i2)) }
  | IF c=condition THEN i=action { BlocCondition(IfThen(c, i)) }

  




condition:
  | TRUE {Val(true)}
  | FALSE {Val(false)}
  | e=expression oC=opComp e2=expression {Comparaison(e, oC, e2)}
  (*On gere le cas de !qqch avant les autres booleens car pas la meme forme*)
  | NOT c=condition { Not(c) }
  | c=condition OR c2=condition {Boolean(c, Or, c2) }
  | c=condition AND c2=condition {Boolean(c, And, c2) }

blocInst:
  | c=action POINTVIRG b=blocInst { Instruction(c, b) }
  | { Fin }

opComp:
    | INF       { Inf }
    | SUP       { Sup }
    | EGALITE   { Egal }
    | NOTEGAL  { NotEgal }
    | INFOREG   { InfOrEg }
    | SUPOREG   { SupOrEg }

opBool:
    | AND   { And }
    | OR    { Or }

couleur:
  NOIR {Noir}
| BLEU {Bleu}
| ROUGE {Rouge}
| VERT {Vert}
| JAUNE {Jaune}
| HEXCOULEUR {Hex($1)}

expression:
   x=NOMBRE{Nombre(x)}
  |MOINS x=NOMBRE {Nombre(-x)}
  |LPAR e=expression RPAR {Parentheses(e)}
  |e=expression MOINS e1=expression {Operation(e,Sub,e1)}
  |MOINSEXP e=expression {Operation(Nombre(0), Sub, e)}
  |e=expression PLUS e1=expression {Operation(e,Add,e1)}
  |e=expression DIV e1=expression {Operation(e,Div,e1)}
  |e=expression FOIS e1=expression {Operation(e,Mult,e1)}
  |s=ID {Ident(s)}


