


(**********************************************
        Projet OCAML S5

  Auteurs : Luxon JEAN-PIERRE & Kahina RAHANI

 **********************************************)

(***********************************************************
  Fichier : Histogramme.ml

  Definition des fonctions de gestion de l'histogramme
 ***********************************************************)


type histo = Couple of (int * int);;


let rec succ_histo oc l_histo = 
  if (oc < 0) || (oc > 255)
  then
    ()
  else
    l_histo.(oc) <- (l_histo.(oc) + 1);;



let rec insert_histo x l = match l with
  | [] -> [x]
  | t::q -> (match x,t with
      | Couple(_,v1),Couple(_,v2) -> 
	if v1 = v2 
	then (x::t::q)
	else if v1 > v2 
	then t::insert_histo x q 
	else (x::t::q));;



let rec sort_histo l = match l with
  | [] -> l
  | [x] -> [x]
  | t::q -> insert_histo t (sort_histo q);;


type 'a option =  Some of 'a | None ;;


let creer_histo entree =
  let rec creer_histo_aux entree l_histo = 
    let ch =
    try
        Some(input_byte(entree))
    with End_of_file -> None 
    in match ch with
      | None -> l_histo 
      | Some(c) -> 
	(succ_histo c l_histo; 
	creer_histo_aux entree l_histo)
  in creer_histo_aux entree (Array.make 256 0);;


let convertir tab =
  let rec convertir_aux tab l i =
    if( i < Array.length tab )
    then if( tab.(i) > 0 ) 
      then convertir_aux tab ((Couple(i,tab.(i)))::l) (i+1)
      else convertir_aux tab l (i+1)
    else l
  in convertir_aux tab [] 0;;









