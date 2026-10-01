open Graphics;;
open ExpDessin;;
let pinceauBaisse = ref false;;
let pinceauCouleur = ref black;;
let pinceauLargeur = ref 1;;
(*pile de (permet de gerer les blocs)*)
(*dico pour les variables, on associe nom de variable à sa valeur(option car pas obligé d'initialiser)*)
let pile : (string, int option) Hashtbl.t list ref =
  ref [Hashtbl.create 10];;

let isInCanva c1 c2 = c1>=0 && c1<=800 && c2>=0 && c2<=800;;

(*fonctions sur la pile*)
let ajout_pile =
  pile := (Hashtbl.create 10) :: !pile

let retirer_pile  =
  match !pile with
  | [] -> failwith "stack vide"
  | _::reste -> pile := reste

let rec find_var name = function
  | [] -> None
  | t::q ->
      (match Hashtbl.find_opt t name with
       | Some v -> Some v
       | None -> find_var name q)

let existe_deja nom = List.exists (fun t -> Hashtbl.mem t nom) !pile

let rec update_var name value = function
  | [] -> failwith ("Variable non déclarée: " ^ name)
  | t::q ->
      if Hashtbl.mem t name then
        Hashtbl.replace t name value
      else
        update_var name value q

let rec eval = function
    |Nombre(n) -> n
    |Parentheses(e) -> (eval e)
    |Operation(e,o,e1) -> (match o with
                        |Add -> (eval e) + (eval e1)
                        |Sub -> (eval e) - (eval e1)
                        |Mult -> (eval e) * (eval e1)
                        |Div when ((eval e1)!=0) -> (eval e) / (eval e1)
                        |_ -> failwith "Division par 0"
    )
    |Ident nom -> (
        (*on cherche la variable dans le dico*)
        (match find_var nom !pile with
        |None -> failwith ("Variable non déclarée: " ^ nom)
        (*on lève une exception si on veut utlisier une var pas initialisée*)
        |Some None -> failwith ("Variable non initialisée: " ^ nom)
        |Some(Some v)-> v
        )
    )
and evalCond = function
    | Val(v) -> v
    | Comparaison(e, oC, e2) -> (match oC with
                            |Inf -> (eval e) < (eval e2)
                            |Sup -> (eval e) > (eval e2)
                            |Egal -> (eval e) = (eval e2)
                            |NotEgal -> (eval e) != (eval e2)
                            |InfOrEg -> (eval e) <= (eval e2)
                            |SupOrEg -> (eval e) >= (eval e2))
    | Not(c) -> not (evalCond c)
    | Boolean(c,oB, c2) -> match oB with
                            | And -> (evalCond c) && (evalCond c2)
                            | Or -> (evalCond c) || (evalCond c2)

    

and dessiner coordonnee1 coordonnee2 direction = function
    (*déclariation variable sans initialisation*)
    | Variable(Var nom) ->
        (*on vérifie si la variable existe déjà*)
        if existe_deja nom
            then failwith ("Variable déjà déclarée: " ^ nom);
         Hashtbl.add (List.hd !pile) nom None;
        (coordonnee1, coordonnee2, direction)
    (*déclaration + initialisation*)
    | Variable(VarDecl(nom, e)) ->
        if existe_deja nom
            then failwith ("Variable déjà déclarée: " ^ nom);
        let v = eval e in
        (*on ajoute la variable dans le dico*)
            Hashtbl.add (List.hd !pile) nom (Some v);
        (coordonnee1, coordonnee2, direction)

    (*initialisation variable déjà déclarée*)
    | Variable(VarInit(nom, e)) -> 
        let v = eval e in
            update_var nom (Some v) !pile;
        (coordonnee1, coordonnee2, direction)
    
    | Variable(Id _) -> failwith "Erreur Id" (*pour pattern matching exhaustif*)
    | Avancer(e) -> let radian = (float_of_int direction) *. Float.pi /. 180.0 in
                    let e_eval = float_of_int (eval e) in
                    let nvC1 = coordonnee1 + int_of_float (e_eval *. cos radian) in
                    let nvC2 = coordonnee2 + int_of_float (e_eval *. sin radian) in
                        set_color !pinceauCouleur;
                        set_line_width !pinceauLargeur;
                        if isInCanva nvC1 nvC2 
                            then (if !pinceauBaisse  then (lineto nvC1 nvC2)
                                    else moveto nvC1 nvC2;
                                        (nvC1, nvC2, direction))
                        else failwith ("Coordonnées en dehors de la toile")
    | LargeurPinceau(e) ->
            let largeur = eval e in
            if largeur <=0 then failwith "Largeur du pinceau négative ou nulle";
            pinceauLargeur := largeur;
            set_line_width !pinceauLargeur; (coordonnee1,coordonnee2,direction)
    | Tourner(e) -> (coordonnee1, coordonnee2 , direction+ (eval e))
    | BaisserLePinceau -> pinceauBaisse := true; (coordonnee1, coordonnee2 , direction)
    | LeverLePinceau -> pinceauBaisse := false; (coordonnee1, coordonnee2 , direction)
    | CouleurPinceau(c) ->
        let couleur_graphics = match c with
            |Noir -> black
            |Bleu -> blue
            |Rouge -> red
            |Vert -> green
            |Jaune -> yellow
                |Hex(s) -> 
                    let r = int_of_string ("0x" ^ String.sub s 0 2) in
                    let g = int_of_string ("0x" ^ String.sub s 2 2) in
                    let b = int_of_string ("0x" ^ String.sub s 4 2) in
                    rgb r g b
    in pinceauCouleur := couleur_graphics;
    set_color !pinceauCouleur; (coordonnee1,coordonnee2,direction)
    |Boucle(b) -> dessinerBoucle coordonnee1 coordonnee2 direction b
    |BlocCondition(c) -> dessinerBlocCond coordonnee1 coordonnee2 direction c
    |Bloc(b) -> interpreterBloc coordonnee1 coordonnee2 direction b

and dessinerBoucle coordonnee1 coordonnee2 direction = function
    | TantQue(c, b) ->let return = ref (coordonnee1, coordonnee2, direction) in 
        while (evalCond c) do 
            ajout_pile;
            let (c1, c2, d) = !return in 
            return := dessiner c1 c2 d b;
            retirer_pile
        done; 
        !return 
    | For(e, i) -> let return = ref (coordonnee1, coordonnee2, direction) in 
        for _=1 to (eval e) do
        ajout_pile;
        let (c1, c2, d) = !return in
        return := dessiner c1 c2 d i;
        retirer_pile;
        done;
        !return

and dessinerBlocCond co1 co2 dir = function
    |IfThen(c, i) -> if (evalCond c) 
        then (ajout_pile;
         let (c1, c2, d) = (dessiner co1 co2 dir i) in
         retirer_pile;
         (c1, c2, d)) else (co1, co2, dir)
    |IfThenElse(c, i, i2) -> if (evalCond c) then
        (ajout_pile;
         let (c1, c2, d) = (dessiner co1 co2 dir i) in
         retirer_pile;
         (c1, c2, d))
      else
        (ajout_pile;
         let (c1, c2, d) = (dessiner co1 co2 dir i2) in 
         retirer_pile;
         (c1, c2, d))

and interpreterBloc coordonnee1 coordonnee2 direction = function
    | Instruction(a, i) -> let (c1,c2, d) = (dessiner coordonnee1 coordonnee2 direction a) in (interpreterBloc c1 c2 d i )
    | Fin -> (coordonnee1, coordonnee2, direction)
    | EndOfFile -> (coordonnee1, coordonnee2, direction);;


