(**********************************************
        Projet OCAML S5

  Auteurs : Luxon JEAN-PIERRE & Kahina RAHANI

 **********************************************)

(*************************************
  Fichier : compresseur.ml

  Fichier en charge de la compression
  ************************************)

(* Appel des bibliothèques *)
open Histogramme;;
open Huffman;;

(*****************************************************
                Ecriture dans le fichier
******************************************************)

exception Code_compresse_invalide;;
exception Caratere_non_reconnu;;

(* Récupère le code compressé du caractère mis en paramètre *)
let getCode c list_code = list_code.(c);;

(* Vérifie si la liste est bien une liste de bit *)
let rec isBit = function
  | [] -> true
  | t::q -> ( (t=1) || (t=0) ) && isBit q;;

(* On injecte bit à bit les éléments de la liste *)
let rec injection_bit sortie = function
  | [] -> ()
  | t::q -> Bitio.output_bit sortie t;injection_bit sortie q ;;

(* On met le code compressé du texte dans le nouveau fichier *)
(* TODO : améliorer ecrire_code *)
let rec ecrire_code sortie code entree = 
  let ch =
    try
      begin
	Some(input_byte entree)
      end
    with End_of_file -> None
  in match ch with
    | None -> injection_bit sortie (getCode 255 code)
    | Some(c) -> 
      let bit_code = getCode c code
      in if isBit bit_code
	then
	  (injection_bit sortie bit_code ;ecrire_code sortie code entree )
	else
	  raise Code_compresse_invalide
;;

(* Stocke l'arbre dans le fichier *)
let rec stockage_arbre sortie tree = match tree with
  | Nil ->() (*output_bit sortie 0; output_bit_byte sortie 255; output_bit sortie 1*)
  | Feuille(Couple(c,n)) -> Bitio.output_bit sortie 0 ; Bitio.output_bit_byte sortie c; 
    if c = 255 then Bitio.output_bit sortie 1 else ()
  | Node(_,g,d) -> Bitio.output_bit sortie 1; stockage_arbre sortie g; stockage_arbre sortie d;;


(* Met n bits après le code compréssé pour s'aligner à la frontière d'octets *)
let put_bits sortie n =
  for i = 1 to n 
  do 
    Bitio.output_bit sortie 0 
  done;;

(* La fonction qui fait la compression, elle prend en parametre une chaine de caractères *)
let compression str_file = 
  let entree = open_in str_file (* On ouvre le fichier *)
  in let histo1 = convertir (creer_histo entree) (* Création histogramme *)
     in let histogramme = insert_histo (Couple(255,1)) histo1 (* Ajout du caractère EOF *)
	in let sorted_histogramme = sort_histo histogramme (* Tri de l'histogramme*)
	   in let liste_arbre = conversion_histoToArbre sorted_histogramme (* Création forêt *)
	      in let huff_tree = construire_Huffman liste_arbre (* Forêt -> arbre Huffman *)
		 in let sortie = Bitio.open_out_bit (str_file^".hf") (* Ouverture fichier .hf *)
		 and tab = Array.make 256 [0] 
		    in construireCode huff_tree tab;
		    Bitio.output_bit_byte sortie 135; Bitio.output_bit_byte sortie 74; (* val magiques*)
		    Bitio.output_bit_byte sortie 31; Bitio.output_bit_byte sortie 72;
		    Bitio.output_bit_byte sortie 0; stockage_arbre sortie huff_tree; (*Octet + arbre*)
		    seek_in entree 0;
		    ecrire_code sortie tab entree; (* On met le code compressé + EOF *)
		    let m = ((sortie.len + 1) mod 8) in if m <> 0 then put_bits sortie (8-m);
		    Bitio.output_bit_byte sortie 0; (* Octet m: içi 0 *)
		    close_in entree; Bitio.close_out_bit sortie;; (*Fermeture du fichier*)



(*let entree = open_in "mots";;
let histogramme = convertir (creer_histo entree);;
let histo2 = insert_histo (Couple('\255',1)) histogramme;;
let sorted_histogramme = sort_histo histo2;;
let liste_arbre =  conversion_histoToArbre sorted_histogramme;;
let huff_tree = construire_Huffman liste_arbre;;
let tab = Array.make 256 [0];;
construireCode huff_tree tab;;

let s = open_out_bit "bla.hf";;
seek_in entree 0;
stockage_arbre s huff_tree;;
ecrire_code s tab entree;; (*BUG *)

close_in entree;;
close_out_bit s;;*)
