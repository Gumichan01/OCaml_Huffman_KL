


(*On a définit un type décrivant l'histogramme *)
type histo = Vide | Couple of (char * int );;

(* Fonctions de recherche et de nombre d'occurrences *)


(* Test si un élément est dans la liste *)
let rec mem x l = match l with
  | [] -> false
  | t::q -> if x = t
    then true
    else mem x q;;

(*Renvoie le nombre d'occurrence d'une lettre dans une liste*)
(*let rec nb_occ a l = match l with
  | [] -> 0
  | t::q -> if a=t then 1 + nb_occ a q else nb_occ a q ;;*)

let  nb_occ a l =
  let rec nb_occ_aux a l n =  match l with
  | [] -> n
  | t::q -> let m = n+1 in if a = t  then nb_occ_aux a q m else nb_occ_aux a q n
  in (nb_occ_aux a l 0);;


(* Génère une liste de toutes les lettres qui sont présentes dans le texte *)
let gen_list_occ txt = 
  let rec gen_list_aux txt occ = match txt with
    | [] -> occ
    | t::q -> if(mem t occ) then gen_list_aux q occ else gen_list_aux q (t::occ)
  in List.rev(gen_list_aux txt []);;



(*Tri de l'histogramme *)

(*Fonction insert_histo *)
let rec insert_histo x l = match l with
  | [] -> [x]
  | t::q -> (match x,t with
      | Vide, Vide -> q
      | Vide, _ -> t::q
      | _, Vide -> insert_histo x q
      | Couple(_,v1),Couple(_,v2) -> if v1 = v2 then (x::t::q)
	else if v1 > v2 then t::insert_histo x q else (x::t::q));;


(* Création de l'histogramme *)
let creer_histo txt lettres =
  let rec creer_histo_aux txt lettres histo = match lettres with
    | [] -> histo
    | t::q ->  creer_histo_aux txt q ((Couple(t , (nb_occ t txt)))::histo)
  in List.rev(creer_histo_aux txt lettres []);;


(* Fonction sort_histo *)
let rec sort_histo l = match l with
  | [] -> l
  | [x] -> [x]
  | t::q -> insert_histo t (sort_histo q);;




