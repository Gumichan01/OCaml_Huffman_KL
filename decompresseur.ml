(**********************************************
        Projet OCAML S5

  Auteurs : Luxon JEAN-PIERRE & Kahina RAHANI

 **********************************************)

(****************************************
  Fichier : decompresseur.ml

  Fichier en charge de la decompression
 ****************************************)

#use "Huffman.ml";;
#use "lib/bitio.ml";;

type 'a option = Some of 'a | None;;

(* Fonction qui crée la liste de 4 octets magiques *)
let creer_liste_magique entree = 
  let rec creer_liste_magique_aux entree l x = 
    let ch =
    try
       Some(input_bit_byte entree)
    with End_of_file -> None
    in match ch with
      | None -> l
      | Some(i) -> if (x <= 4) then  creer_liste_magique_aux entree (l@[i]) (x+1) else l
  in creer_liste_magique_aux entree [] 1;;


(* Fonction qui compare deux liste *)
let rec comparaison l1 l2 = match l1,l2 with
  | [],[] -> true
  | [],_ -> false
  | _,[] -> false
  | a1::q1,a2::q2 -> (a1 = a2) && comparaison q1 q2;;


(* Construit l'arbre à partir des bis - TODO, gérer l'exception End_of_file *)
let rec construire_arbreH entree = 
  let bit_lu = input_bit entree
  in if bit_lu = 0 
    then (let ch = (char_of_int (input_bit_byte entree))
	 in 
	 (match ch with
	   | '\255' -> ignore (input_bit entree);Feuille(Couple(ch,0))
	   | _ -> Feuille(Couple(ch,0)) ))
    else
      let tmp = (construire_arbreH entree)
      in let tmp1 = (construire_arbreH entree)
      in fusion tmp tmp1;;

let ecrire_dans_fichier entree arbre sortie = 
  let rec ecrire_dans_fichier_aux entree arbre root sortie =
    (match arbre with (* On regarde si on a une feuille *)
      | Nil -> failwith "Arbre non valide " (* Interdit, echec *)
      | Feuille(Vide) -> failwith "ERREUR" (* Interdit , echec *)
      | Feuille(Couple(c,_)) -> (* OK *)
	(if c = '\255' (* Fin du fichier *)
	 then raise Exit
	 else  (output_char sortie c; ecrire_dans_fichier_aux entree root root sortie;) )
      | Node(_,_,_) -> ());
    let bit =
    try
       Some(input_bit entree)
    with End_of_file -> None
    in match bit with
      | None -> failwith "Fin du fichier prématurée "
      | Some(b) -> (match arbre with
	  | Nil -> failwith "Arbre non valide "
	  | Feuille(_) -> failwith "ERREUR"
	  | Node(_,g,d) -> 
	    (if b = 0 then ecrire_dans_fichier_aux entree g root sortie
	    else ecrire_dans_fichier_aux entree d root sortie))
  in ecrire_dans_fichier_aux entree arbre arbre sortie
;;



(* Fait la décompression, La fonction marche est est interopérable *)
(* En revanche j'ai un doute pour le input_align_bit*)
(* ça marche bien parce que l'octet à ignorer vaut 0*)
(* Mais que ce passe-il si l'octet vaut n, tel que [0 < n < +infini] ? Pas sûr que ça marche !*)
(* En plus, d'après le code du prof, le champs len est tel que [0 <= len  <= 8] *)
(* En fait il aligne le curseur sur une fronière d'octet comme on l'avait fait pour compresser*)
(* Donc à mon avis, on devra faire une fonction appelant n fois input_align_bit *)
(* TODO : Enlever la concatenation et avoir un fichier de sortie sans le ".hf" *)
(* TODO : Exemple, si je decompresse "toto.txt.hf" mon fichier de sortie sera "toto.txt" *)
let decompression str_file = 
  let octetsMagiques = [135;74;31;72] and entree = open_in_bit str_file (*On ouvre le fichier*)
  in
  try 
    let valide =  comparaison octetsMagiques (creer_liste_magique entree)
    in
    if (valide = false) 
    then (print_string "Erreur : Format du fichier non valide \n"; raise Exit) 
    else input_align_bit entree; (* J'ai un doute pour ce input_align_bit *)
    let arbre = construire_arbreH entree
    in let sortie = open_out (str_file^".txt") (* enlever la concatenation*)
       in 
       try ecrire_dans_fichier entree arbre sortie
       with Exit -> input_align_bit entree (*OK c'est normal*);close_in_bit entree; close_out sortie
  with End_of_file -> print_string "Erreur fatal, format invalide";;


decompression "ab.hf";;
decompression "fichier_lib.ml.hf";;
decompression "projet_ocaml_s5.tar.gz";;
