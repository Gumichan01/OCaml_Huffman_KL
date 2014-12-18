
(**********************************************
        Projet OCaml S5

  Auteurs : Luxon JEAN-PIERRE & Kahina RAHANI

 **********************************************)

(*************************************
  Fichier : compresseur.ml

  Fichier en charge de la compression
  ************************************)


(* #use "Histogramme.ml";;
 #use "Huffman.ml";;*)

open Histogramme;;
open Huffman;;

(*****************************************************
                Ecriture dans le fichier
******************************************************)

exception  Code_compresse_invalide;;

let getCode c list_code = list_code.(c);;


let rec isBit = function
  | [] -> true
  | t::q -> ( (t=1) || (t=0) ) && isBit q;;

(* On injecte bit à bit les éléments de la liste *)
let rec injection_bit sortie = function
  | [] -> ()
  | t::q -> Bitio.output_bit sortie t;injection_bit sortie q ;;

(* On met le code compressé du texte dans le nouveau fichier *)
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
  | Feuille(Couple(c,n)) ->
    Bitio.output_bit sortie 0 ; Bitio.output_bit_byte sortie c; 
    if c = 255 
    then Bitio.output_bit sortie 1 
    else ()
  | Node(_,g,d) -> 
    Bitio.output_bit sortie 1; stockage_arbre sortie g; stockage_arbre sortie d;;


let compression str_file = 
  let entree = open_in str_file in 
  let histo1 = convertir (creer_histo entree) in 
  let histogramme = insert_histo (Couple(255,1)) histo1 in 
  let sorted_histogramme = sort_histo histogramme in 
  let liste_arbre = conversion_histoToArbre sorted_histogramme in 
  let huff_tree = construire_Huffman liste_arbre in 
  let sortie = Bitio.open_out_bit (str_file^".hf")
  and tab = Array.make 256 [0] in 
  construireCode huff_tree tab;
  Bitio.output_bit_byte sortie 135; Bitio.output_bit_byte sortie 74;
  Bitio.output_bit_byte sortie 31; Bitio.output_bit_byte sortie 72;
  Bitio.output_bit_byte sortie 0; stockage_arbre sortie huff_tree;
  seek_in entree 0;
  ecrire_code sortie tab entree;
  Bitio.output_align_bit sortie;
  Bitio.output_bit_byte sortie 0;
  close_in entree; Bitio.close_out_bit sortie;;


compression "data/abracadabra.txt";;


