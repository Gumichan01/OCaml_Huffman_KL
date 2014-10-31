
(* Appel des bibliothèques *)
#use "fichier_lib.ml";;
#use "Huffman.ml";;
#use "bitio.ml";;


(* main partie 1*)
let texte = lire_fichier "data/abracadabra.txt";;
let histogramme = creer_histo texte (gen_list_occ (texte));;
let sorted_histogramme = sort_histo histogramme;;
let liste_arbre =  conversion_histoToArbre sorted_histogramme;;
let huff_tree = construire_Huffman liste_arbre;;


(*****************************************************
                Ecriture dans le fichier
******************************************************)

exception Code_compresse_invalide;;
exception Caratere_non_reconnu;;

(* Récupère le code compressé du caractère mis en paramètre *)
let rec getCode c list_code = match list_code with
  | [] -> []
  | t::q -> (match t with
      | Code(s,l_int) -> if s = c then l_int else getCode c q);;

(* Vérifie si la liste est bien une liste de bit *)
let rec isBit = function
  | [] -> true
  | t::q -> ( (t=1) || (t=0) ) && isBit q;;

(* On injecte bit à bit les éléments de la liste *)
let injection_bit sortie = function
  | [] -> ()
  | t::q -> output_bit sortie t ;;

(* On met le code compressé du texte dans le nouveau fichier *)
let rec ecrire_code sortie code texte = match texte with
  | [] -> ()
  | t::q -> let bit_code = getCode t code in 
	    if isBit bit_code 
	    then (injection_bit sortie bit_code ;ecrire_code sortie code q )
	    else raise Code_compresse_invalide ;;

(* Stocke l'arbre dans le fichier*)
let rec stockage_arbre sortie tree = match tree with
  | Nil -> failwith "Arbre vide : echec de la compression "
  | Feuille(f) -> (match f with
      | Vide -> failwith "Feuille Vide : echec de la compression"
      | Couple(c,n) -> output_bit sortie 0 ; output_bit_byte sortie (Char.code c) )
  | Node(_,g,d) -> output_bit sortie 1; stockage_arbre sortie g; stockage_arbre sortie d;;


(* main partie 2 *)
let sortie = open_out_bit "test.hf";;
output_bit_byte sortie 135;; (* Les octects "magiques" *)
output_bit_byte sortie 74;;
output_bit_byte sortie 31;;
output_bit_byte sortie 72;;
output_bit_byte sortie 0;; (* taille des octects ignorés *)
stockage_arbre sortie huff_tree;;
ecrire_code sortie (construireCode huff_tree)  texte;;
output_bit_byte sortie 255;; (*Fin du fichier*)
output_bit sortie 1;;
(*output_bit sortie 0;;*)
close_out_bit sortie;;

Char.code 't';;

(*let entree = open_in_bit "test.hf";;
let v = input_bit_byte entree;;
close_in_bit entree;;*)


















