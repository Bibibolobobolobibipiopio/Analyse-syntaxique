
open Graphics

let lexbuf = Lexing.from_channel stdin 

let dessin inter =
  (* Ouverture d'une fenêtre graphique *)
  open_graph ""; (* Attention à l'espace au début *)
  resize_window  800 800;
  set_window_title "Dessin incroyable";

  set_line_width 1;
  set_color black;
  moveto 400 400;
  ignore (Dessiner.interpreterBloc 400 400 90 inter);
  (* Attend l'appui d'une touche et ferme la fenêtre *)
  ignore (read_key ());
  close_graph ()

           
let ast = Parser.input Lexer.token lexbuf 
let _ = dessin ast