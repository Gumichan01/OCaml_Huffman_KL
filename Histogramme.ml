(**********************************************
        Projet OCAML S5

  Auteurs : Luxon JEAN-PIERRE & Kahina RAHANI

 **********************************************)

(***********************************************************
  Fichier : Huistogramme.ml

  Definition des fonctions de gestion de l'histogramme
 ***********************************************************)


(*On a définit un type décrivant l'histogramme *)
type histo = Couple of (char * int );;

(* Fonctions de recherche et de nombre d'occurrences *)


(* Verifie si une lettre est dans l'histogramme *)
(*let rec mem_histo e l = match l with
  | [] -> false
  | t::q -> (match t with
      | Couple(c,_) -> if c = e then true else mem_histo e q);;*)


(* [|1;2;3;4|];; *)

(* Mets à jour la valeur d'un octet ("caractère") de l'histogramme *)
let rec succ_histo oc l_histo = 
  if (oc < 0) || (oc > 255)
  then
    ()
  else
    l_histo.(oc) <- (l_histo.(oc) + 1);;

let tab = [|0;0;0;0;0|]
in succ_histo 2 tab; tab;;

(*Tri de l'histogramme *)

(*Fonction insert_histo *)
let rec insert_histo x l = match l with
  | [] -> [x]
  | t::q -> (match x,t with
      | Couple(_,v1),Couple(_,v2) -> 
	if v1 = v2 
	then (x::t::q)
	else if v1 > v2 
	then t::insert_histo x q 
	else (x::t::q));;


(* Fonction sort_histo *)
let rec sort_histo l = match l with
  | [] -> l
  | [x] -> [x]
  | t::q -> insert_histo t (sort_histo q);;


type 'a option =  Some of 'a | None ;;

(* On crée l'histogramme dynamiquement en parcourant le fichier *)
let creer_histo entree =
  let rec creer_histo_aux entree l_histo = 
    let ch =
    try
        Some(input_char(entree))
    with End_of_file -> None 
    in match ch with
      | None -> l_histo 
      | Some(c) -> 
	(succ_histo (int_of_char c) l_histo; 
	creer_histo_aux entree l_histo)
  in creer_histo_aux entree (Array.make 256 0);;

(* Convertir un tableau en une liste *)
let convertir tab =
  let rec convertir_aux tab l i =
    if( i < Array.length tab )
    then if( tab.(i) > 0 ) 
      then convertir_aux tab ((Couple((char_of_int i),tab.(i)))::l) (i+1)
      else convertir_aux tab l (i+1)
    else l
  in convertir_aux tab [] 0;;

convertir [|0;5;3;1;95;8245;1;364;25;0;78;6;2;0;52;42;0;0;4|];;


let fp = open_in "mots";;
let h = creer_histo fp;;
convertir h;;
close_in fp;;









