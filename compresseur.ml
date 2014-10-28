
(* Appel des bibliothèques *)
#use "fichier_lib.ml";;
#use "Huffman.ml";;
#use "bitio.ml";;


let texte = lire_fichier "data/abracadabra.txt";;
let histogramme = creer_histo texte (gen_list_occ (texte));;
let sorted_histogramme = sort_histo histogramme;;
let liste_arbre =  conversion_histoToArbre sorted_histogramme;;
let huff_tree = construire_Huffman liste_arbre;;


(*****************************************************
                Ecriture dans le ficier
******************************************************)


(*let tmp =  open_in_bit "data/abracadabra.txt";;
close_in_bit tmp;;*)

(*let rec magic_input f l = match l with
  | [] -> l
  | t::q -> output_bit f t; magic_input f q;;*)

let rec stockage_arbre sortie tree = match tree with
  | Nil -> failwith "Arbre vide : echec de la compression "
  | Feuille(f) -> (match f with
      | Vide -> failwith "Feuille Vide : echec de la compression"
      | Couple(c,n) -> output_bit sortie 0 ; output_bit_byte sortie (Char.code 't') )
  | Node(_,g,d) -> output_bit sortie 1; stockage_arbre sortie g; stockage_arbre sortie d;;

let sortie = open_out_bit "test.hf";;
output_bit_byte sortie 135;; (* Les octects "magiques" *)
output_bit_byte sortie 74;;
output_bit_byte sortie 31;;
output_bit_byte sortie 72;;
output_bit_byte sortie 0;; (* taille des octects ignorés *)
stockage_arbre sortie huff_tree;;
output_bit_byte sortie 255;;
output_bit sortie 1;;
close_out_bit sortie;;

Char.code 't';;

let entree = open_in_bit "test.hf";;
let v = input_bit_byte entree;;
close_in_bit entree;;


















