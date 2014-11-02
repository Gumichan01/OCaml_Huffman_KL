
(* Appel des bibliothèques *)
#use "fichier_lib.ml";;
#use "Huffman.ml";;
#use "bitio.ml";;


(* main partie 1*)
(*let texte = lire_fichier "data/duo.txt";;
let histogramme = creer_histo texte (gen_list_occ (texte));;
let sorted_histogramme = sort_histo histogramme;;
let liste_arbre =  conversion_histoToArbre sorted_histogramme;;
let huff_tree = construire_Huffman liste_arbre;;
let result = construireCode huff_tree;;
let codeBuild = construireCode huff_tree;;*)

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
let rec injection_bit sortie = function
  | [] -> ()
  | t::q -> output_bit sortie t;injection_bit sortie q ;;

(* On met le code compressé du texte dans le nouveau fichier *)
let rec ecrire_code sortie code texte = match texte with
  | [] -> injection_bit sortie (getCode '\255' code)
  | t::q -> let bit_code = getCode t code in 
	    if isBit bit_code 
	    then (injection_bit sortie bit_code ;ecrire_code sortie code q )
	    else raise Code_compresse_invalide ;;

(* Stocke l'arbre dans le fichier*)
let rec stockage_arbre sortie tree = match tree with
  | Nil ->() (*output_bit sortie 0; output_bit_byte sortie 255; output_bit sortie 1*)
  | Feuille(f) -> (match f with
      | Vide -> failwith "Feuille Vide : echec de la compression"
      | Couple(c,n) -> output_bit sortie 0 ; output_bit_byte sortie (Char.code c); 
	if c = '\255' then output_bit sortie 1 else () )
  | Node(_,g,d) -> output_bit sortie 1; stockage_arbre sortie g; stockage_arbre sortie d;;


(* main partie 2 *)
(*let sortie = open_out_bit "test.hf";;
output_bit_byte sortie 135;; 
output_bit_byte sortie 74;;
output_bit_byte sortie 31;;
output_bit_byte sortie 72;;
output_bit_byte sortie 0;;
stockage_arbre sortie huff_tree;;
ecrire_code sortie (construireCode huff_tree)  texte;;
output_bit_byte sortie 255;;
output_bit sortie 1;;
print_int sortie.len;;
close_out_bit sortie;;


Char.code '\255';;
let x = char_of_int 255;;*)


(* Met n bits après le code compréssé pour s'aligner à la frontière d'octets *)
let put_bits sortie n =
  for i = 1 to n 
  do 
    output_bit sortie 0 
  done;;


let compression str_file = 
  let texte = lire_fichier str_file
  in let histo1 = creer_histo texte (gen_list_occ (texte))
     in let histogramme = insert_histo (Couple('\255',1)) histo1
	in let sorted_histogramme = sort_histo histogramme
	   in let liste_arbre = conversion_histoToArbre sorted_histogramme
	      in let huff_tree = construire_Huffman liste_arbre
		 in let sortie = open_out_bit (str_file^".hf") 
		 and codeBuild = construireCode huff_tree 
		    in output_bit_byte sortie 135; output_bit_byte sortie 74; (*Octets magiques*)
		    output_bit_byte sortie 31; output_bit_byte sortie 72;
		    output_bit_byte sortie 0; stockage_arbre sortie huff_tree; (*Octet n + arbre*)
		    ecrire_code sortie codeBuild texte; (* On met le code compressé *)
		    (*output_bit sortie 0; output_bit_byte sortie 255;output_bit sortie 1;*)(*EOF*)
		    let m = ((sortie.len + 1) mod 8) in if m <> 0 then put_bits sortie (8-m);
		    output_bit_byte sortie 0; (* Octet m: içi 0 *)
		    close_out_bit sortie;;

(*compression "data/abracadabra.txt";;*)
compression "/home/luxon/cours/ocaml/exe_dir/test_en";;
(*compression "duo.txt";;*)
(*compression "/home/luxon/cours/ocaml/exe_dir/toto.txt";;*)

(*print_string "hello world";;*)




