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
let rec mem_histo e l = match l with
  | [] -> false
  | t::q -> (match t with
      | Couple(c,_) -> if c = e then true else mem_histo e q);;


let rec succ_histo ch l_histo = match l_histo with
  | [] -> l_histo
  | t::q -> (match t with
      | Couple(c,v) -> if c = ch then (Couple(c,(fun x-> x+1) v))::q else t::(succ_histo ch q) );;


(*Tri de l'histogramme *)

(*Fonction insert_histo *)
let rec insert_histo x l = match l with
  | [] -> [x]
  | t::q -> (match x,t with
      | Couple(_,v1),Couple(_,v2) -> if v1 = v2 then (x::t::q)
	else if v1 > v2 then t::insert_histo x q else (x::t::q));;

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
      | Some(c) ->  if mem_histo c l_histo then creer_histo_aux entree (succ_histo c l_histo)  else creer_histo_aux entree ((Couple(c,1))::l_histo) (* -> Option à mettre *)
  in creer_histo_aux entree [];;


(* Fonction sort_histo *)
let rec sort_histo l = match l with
  | [] -> l
  | [x] -> [x]
  | t::q -> insert_histo t (sort_histo q);;







