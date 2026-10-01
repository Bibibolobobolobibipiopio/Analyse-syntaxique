{
  open Parser
}
let digit = ['0'-'9']
let nombre = digit+
let hex_digit = digit | ['a'-'f' 'A'-'F']
(*couleur hexadécimale sur 6 caractères*)
let hex_color = hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit
(*id commence par lettre min ou maj puis caractères alphanuméériques*)
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
  | eof { EOF }
  |['a' 'A']"vancer" {AVANCER}
  |['t' 'T']"ourner"{TOURNER}
  |['b' 'B']"aisserPinceau" {BAISSERPIN}
  |['l' 'L']"everPinceau" {LEVERPIN}
  |['c' 'C']"ouleurPinceau" {COULEURPINCEAU}
  |['l' 'L']"argeurPinceau" {LARGEURPINCEAU}
  | "Tant que" {WHILE}
  | "Faire" {FAIRE}
  | "Repeter" {FOR}
  | "Fois" {DEFOIS}
  | "Debut" {DEBUT}
  | "Fin" {FIN}
  | "Si" {IF}
  | "Alors" {THEN}
  | "Sinon" {ELSE}
  |"Var" {VAR}
  |"noir" {NOIR}
  |"bleu" {BLEU}
  |"rouge" {ROUGE}
  |"vert" {VERT}
  |"jaune" {JAUNE}
  |hex_color as hex { HEXCOULEUR(hex) }
  |nombre {NOMBRE(int_of_string (Lexing.lexeme lexbuf))}
  |'(' {LPAR}
  |')' {RPAR}
  |'+' {PLUS}
  |'-' {MOINS}
  |'*' {FOIS}
  |'/' {DIV}
  |';' {POINTVIRG}
  | "==" {EGALITE}
  | "!=" {NOTEGAL}
  | "<=" {INFOREG}
  |">=" {SUPOREG}
  |'=' {EGAL}
  | '<' {INF}
  | '>' {SUP}
  | "&&" {AND}
  | "||" {OR}
  | '!' {NOT}
  | "true" {TRUE}
  | "false" {FALSE}
  |id as identifiant {ID(identifiant)}
  | [' ' '\t' '\r' '\n']+ { token lexbuf }  (* retour a la ligne et espaces pour que menhir ne panique pas*)
  |_ { failwith "caractère pas dans le langage" }


