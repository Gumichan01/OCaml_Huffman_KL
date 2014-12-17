(**********************************************
        Projet OCAML S5

  Auteurs : Luxon JEAN-PIERRE & Kahina RAHANI

 **********************************************)

(***********************************************************
  Fichier : Huffman.ml

  Definition des fonctions de gestion de l'arbre de Huffman
 ***********************************************************)

(*#use "Histogramme.ml";;*)
open Histogramme;;


type arbreH = 
  | Feuille of histo
  | Node of int * arbreH * arbreH;;


let rec insert_tree a l = match l with
  | [] -> [a]
  | t::q -> 
    (match a, t with 
      | Feuille(cpa), Feuille(cpt) -> (match cpa, cpt with
	  | Couple(_,i1), Couple(_,i2) -> 
	    if i1 = i2 then a::t::q
	    else if i1 < i2 then a::t::q
	    else t ::(insert_tree a q))
      | Feuille(cpa), Node(n,_,_)-> (match cpa with
	  | Couple(_,i) -> 
	    if n = i then a::t::q 
	    else if n < i then t::(insert_tree a q)
	    else a::t::q)
      | Node(n,_,_), Feuille(cpt) ->
	(match cpt with 
	  | Couple(_, i) -> 
	    if i = n then a::t::q
	    else if n < i then a::t::q
	    else t::(insert_tree a q)
	)
      | Node(n1,_,_) , Node(n2,_,_) -> 
	if n1 = n2 then a::t::q
	else if n1 < n2 then a::t::q
	else t::(insert_tree a q)
    )
;;


exception Histo_vide;;


let fusion a1 a2= match a1,a2 with
  | Feuille(cp1) , Feuille(cp2) -> (match cp1,cp2 with
      | Couple(c1,i1),Couple(c2,i2) -> 
	Node( (i1+i2) , Feuille(cp1), Feuille(cp2) ))
  | Feuille(cp1), Node(n,t1, t2) -> 
    (match cp1 with
      | Couple(_, i1) -> Node((n + i1),a1,a2)
    )
  | Node(n, t1, t2), Feuille(cp2) -> 
    (match cp2 with
      |Couple(_, i2) -> Node((n + i2),a1,a2)
    )
  |Node(n1,_,_) , Node(n2, _, _) -> Node( (n1 + n2), a1, a2)
;;


let rec conversion_histoToArbre l_histo = match l_histo with
  | [] -> []
  | t::q -> Feuille(t)::(conversion_histoToArbre q);;


let rec construire_Huffman = function
  | [] -> raise Histo_vide
  | [t] -> t
  | t1::t2::q -> construire_Huffman(insert_tree (fusion t1 t2) q);;


type codeCompress = Code of (int * int list);;


let construireCode arbre tab =
  let rec construireCode_aux arbre l_bit l_code = match arbre with
    | Feuille(Couple(c,_))-> l_code.(c) <- l_bit;
    | Node(_,g,d) -> 
      let lb = l_bit in (construireCode_aux g (lb@[0]) l_code);
      (construireCode_aux d (l_bit@[1]) l_code )
  in construireCode_aux arbre [] tab;;




