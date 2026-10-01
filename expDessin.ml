type blocInst = 
    |Instruction of action * blocInst
    |Fin
    |EndOfFile

and action = 
    | BaisserLePinceau
    | LeverLePinceau
    | Avancer of expression
    | Tourner of expression
    | CouleurPinceau of couleur
    | LargeurPinceau of expression
    | Variable of variable
    | Boucle of boucle
    | BlocCondition of blocCond
    | Bloc of blocInst



and boucle = 
    | TantQue of condition *  action
    | For of expression * action

and blocCond = 
    |IfThen of condition * action
    |IfThenElse of condition * action * action
    
and condition =
    | Val of bool
    | Comparaison of expression * opComp * expression
    | Not of condition
    | Boolean of condition * opBool * condition

and opComp =
    |Inf
    |Sup
    |Egal
    |NotEgal
    |InfOrEg
    |SupOrEg

and opBool = 
    | And
    | Or

and variable = 
    |Id of string
    |Var of string
    |VarDecl of string * expression
    |VarInit of string * expression

and couleur =
    | Noir
    | Bleu
    | Rouge
    | Vert
    | Jaune
    | Hex of string

and expression = 
    | Nombre of int
    | Parentheses of expression
    | Operation of expression * operateur * expression
    | Ident of string

and operateur = 
    | Add
    | Sub
    | Mult
    | Div

