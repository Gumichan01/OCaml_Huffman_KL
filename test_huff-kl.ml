
(* Test des fichiers *)

#use "Histogramme.ml";;
#use "Huffman.ml";;
#use "compresseur.ml";;


let tab = [|0;0;0;0;0|]
in succ_histo 2 tab; tab;;

convertir [|0;5;3;1;95;8245;1;364;25;0;78;6;2;0;52;42;0;0;4|];;


(* Test d'insertion dans un arbre *)
insert_tree Nil [];;
insert_tree (Feuille(Couple('t', 8))) [];;
insert_tree (Feuille(Couple('a', 4))) [Nil];;
insert_tree Nil [(Feuille(Couple('H', 2)))] ;;

insert_tree (Feuille(Vide)) [(Feuille(Vide))];;
insert_tree (Feuille(Vide)) [(Feuille(Couple('s', 1)))];;
insert_tree (Feuille(Couple('t', 1))) [(Feuille(Vide))];;

insert_tree (Feuille(Couple('t', 8))) [(Feuille(Couple('f', 3)))];;
insert_tree (Feuille(Couple('t', 2))) [(Feuille(Couple('f', 3)))];;

insert_tree (Feuille(Couple('t', 12))) [(Node(10,(Feuille(Couple('A', 7))),Feuille(Couple('t',3)) ))];;
insert_tree (Feuille(Vide)) [(Node(9,(Feuille(Couple('A', 7))),Feuille(Couple('t',3)) ))];;

insert_tree (Node(10,(Feuille(Couple('A', 7))),Feuille(Couple('t',3)) )) [(Feuille(Couple('t', 1)))];;
insert_tree (Node(10,(Feuille(Couple('A', 7))),Feuille(Couple('t',3)) )) [(Feuille(Vide))];;

insert_tree (Node(10,(Feuille(Couple('A', 7))),Feuille(Couple('t',3)) )) [(Node(12,(Feuille(Couple('A', 6))),Feuille(Couple('t',6)) ))];;


(* type Code *)
Code('a', [1;0]);;







