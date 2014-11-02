


(*On a définit un type décrivant l'histogramme *)
type histo = Vide | Couple of (char * int );;

(* Fonctions de recherche et de nombre d'occurrences *)


(* Test si un élément est dans la liste *)
let rec mem x l = match l with
  | [] -> false
  | t::q -> if x = t
    then true
    else mem x q;;

(* Verifie si une lettre est dans l'histogramme *)
let rec mem_histo e l = match l with
  | [] -> false
  | t::q -> (match t with
      | Vide -> mem_histo e q
      | Couple(c,_) -> if c = e then true else mem_histo e q);;

(*Renvoie le nombre d'occurrence d'une lettre dans une liste*)
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


let rec succ_histo ch l_histo = match l_histo with
  | [] -> l_histo
  | t::q -> (match t with
      | Vide -> l_histo
      | Couple(c,v) -> if c = ch then (Couple(c,(fun x-> x+1) v))::l_histo else t::(succ_histo ch q) );;


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


(* On crée l'histogramme dynamiquement en parcourant le fichier *)
let creer_histo entree =
  let rec creer_histo_aux entree l_histo = 
    try
       let ch  = input_char(entree) 
       in if mem_histo ch l_histo then creer_histo_aux entree (succ_histo ch l_histo)  else creer_histo_aux entree ((Couple(ch,1))::l_histo)
    with End_of_file -> l_histo 
  in creer_histo_aux entree [];;


(* Fonction sort_histo *)
let rec sort_histo l = match l with
  | [] -> l
  | [x] -> [x]
  | t::q -> insert_histo t (sort_histo q);;







