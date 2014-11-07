(**********************************************
        Projet OCAML S5

  Auteurs : Luxon JEAN-PIERRE & Kahina RAHANI

 **********************************************)

(*************************************
  Fichier : compresseur.ml

  Fichier en charge de la compression
  ************************************)

(* Appel des bibliothèques *)
#use "Huffman.ml";;
#use "lib/bitio.ml";;



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
let rec ecrire_code sortie code entree = 
  try
    begin
      let ch = input_char entree 
      in let bit_code = getCode ch code 
	 in if isBit bit_code
	   then
	     (injection_bit sortie bit_code ;ecrire_code sortie code entree )
	   else
	     raise Code_compresse_invalide
    end
  with End_of_file -> injection_bit sortie (getCode '\255' code);;



(* Stocke l'arbre dans le fichier *)
let rec stockage_arbre sortie tree = match tree with
  | Nil ->() (*output_bit sortie 0; output_bit_byte sortie 255; output_bit sortie 1*)
  | Feuille(f) -> (match f with
      | Vide -> failwith "Feuille Vide : echec de la compression"
      | Couple(c,n) -> output_bit sortie 0 ; output_bit_byte sortie (Char.code c); 
	if c = '\255' then output_bit sortie 1 else () )
  | Node(_,g,d) -> output_bit sortie 1; stockage_arbre sortie g; stockage_arbre sortie d;;


(* Met n bits après le code compréssé pour s'aligner à la frontière d'octets *)
let put_bits sortie n =
  for i = 1 to n 
  do 
    output_bit sortie 0 
  done;;

(* La fonction qui fait la compression, elle prend en parametre une chaine de caractères *)
let compression str_file = 
  let entree = open_in str_file (* On ouvre le fichier *)
  in let histo1 = creer_histo entree (* Création histogramme *)
     in let histogramme = insert_histo (Couple('\255',1)) histo1 (* Ajout du caractère EOF *)
	in let sorted_histogramme = sort_histo histogramme (* Tri de l'histogramme*)
	   in let liste_arbre = conversion_histoToArbre sorted_histogramme (* Création forêt *)
	      in let huff_tree = construire_Huffman liste_arbre (* Forêt -> arbre Huffman *)
		 in let sortie = open_out_bit (str_file^".hf") (* Ouverture fichier .hf *)
		 and codeBuild = construireCode huff_tree (* Construire code compresse *)
		    in output_bit_byte sortie 135; output_bit_byte sortie 74; (* val magiques*)
		    output_bit_byte sortie 31; output_bit_byte sortie 72;
		    output_bit_byte sortie 0; stockage_arbre sortie huff_tree; (*Octet + arbre*)
		    seek_in entree 0;
		    ecrire_code sortie codeBuild entree; (* On met le code compressé + EOF *)
		    let m = ((sortie.len + 1) mod 8) in if m <> 0 then put_bits sortie (8-m);
		    output_bit_byte sortie 0; (* Octet m: içi 0 *)
		    close_in entree; close_out_bit sortie;; (*Fermeture du fichier*)


(*compression "data/abracadabra.txt";;*)
(*compression "/home/luxon/cours/ocaml/exe_dir/test_en";;*)
(*compression "duo.txt";;*)
compression "fichier_lib.ml";;
(*compression "/home/luxon/cours/ocaml/exe_dir/toto.txt";;*)

(*print_string "hello world";;*)




