
(* Traitment sur le fichier *)


(* Lire le fichier caractère par caractère *)
let rec lire_char f = 
  try
    let ch  = input_char(f) in ch::lire_char(f)     
 with End_of_file -> close_in f; [];;


(*Ouvrir un fichier et lire son contenu*)
let lire_fichier str_file = 
  let f = open_in str_file in
  lire_char(f);;

