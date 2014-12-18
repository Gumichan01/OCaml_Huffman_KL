
(**********************************************
        Projet OCaml S5

  Auteurs : Luxon JEAN-PIERRE & Kahina RAHANI

 **********************************************)

(****************************************
  Fichier : decompresseur.ml

  Fichier en charge de la decompression
 ****************************************)

(*#use "Huffman.ml";;
 #use "lib/bitio.ml";;*)

open Histogramme;;
open Huffman;;

type 'a option = Some of 'a | None;;

(* Fonction qui crée la liste de 4 octets magiques  @todo*)
let creer_liste_magique entree = 
  let rec creer_liste_magique_aux entree l x = 
    if( x > 4) 
    then 
      l
    else
      let ch =
	try
	  Some(Bitio.input_bit_byte entree)
	with End_of_file -> None
      in match ch with
	| None -> l
	| Some(i) -> 
	  creer_liste_magique_aux entree (l@[i]) (x+1) 
  in creer_liste_magique_aux entree [] 1;;



let rec comparaison l1 l2 = match l1,l2 with
  | [],[] -> true
  | [],_ -> false
  | _,[] -> false
  | a1::q1,a2::q2 -> (a1 = a2) && comparaison q1 q2;;



let rec construire_arbreH entree = 
  let bit_lu = Bitio.input_bit entree
  in if bit_lu = 0 
    then 
      (let ch = Bitio.input_bit_byte entree in 
       (match ch with
	 | 255 -> ignore (Bitio.input_bit entree);Feuille(Couple(ch,0))
	 | _ -> Feuille(Couple(ch,0)) 
       )
      )
    else
      let tmp = (construire_arbreH entree) in 
      let tmp1 = (construire_arbreH entree) in 
      fusion tmp tmp1;;


let ecrire_dans_fichier entree arbre sortie = 
  let rec ecrire_dans_fichier_aux entree arbre root sortie =
    (match arbre with
      | Feuille(Couple(c,_)) ->
	(if c = 255 
	 then
	    raise Exit
	 else 
	    output_byte sortie c; ecrire_dans_fichier_aux entree root root sortie;
	)
      | Node(_,g,d) -> 
	let bit =
	  try
	    Some(Bitio.input_bit entree)
	  with End_of_file -> None
	in match bit with
	  | None -> 
	    failwith "Fin du fichier prématurée "
	  | Some(b) -> 
	    (if b = 0 
	     then ecrire_dans_fichier_aux entree g root sortie
	     else ecrire_dans_fichier_aux entree d root sortie
	    )
    )
     in ecrire_dans_fichier_aux entree arbre arbre sortie
;;


let ignorer_octets entree n =
  let i = ref n in
  while (!i) <> 0 
  do
    ignore(Bitio.input_bit_byte entree);
    i := (!i) - 1;
  done
;;


let decompression str_file = 
  let octetsMagiques = [135;74;31;72] 
  and entree = Bitio.open_in_bit str_file 
  and sortie = open_out (String.sub str_file 0 ((String.length str_file) - 3) ) in
  try
    let valide =  comparaison octetsMagiques (creer_liste_magique entree) in
    if (valide = false) 
    then 
      (print_string "Erreur : Entête de fichier non valide !! \n"; raise Exit)
    else 
      (let n = Bitio.input_bit_byte entree in
       ignorer_octets entree n;
       let arbre = construire_arbreH entree in
       try
	 ecrire_dans_fichier entree arbre sortie
       with 
	   Exit -> Bitio.input_align_bit entree; close_out sortie; 
	     ignorer_octets entree (Bitio.input_bit_byte entree);
	     Bitio.close_in_bit entree;
      ) 
  with End_of_file ->
    print_string "Erreur : Fichier invalide !! \n"; 
    close_out sortie;
    Bitio.close_in_bit entree;;

(*decompression "abracadabra.hf";;
decompression "toto.hf";;*)
