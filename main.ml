(**********************************************
        Projet OCaml S5

  Auteurs : Luxon JEAN-PIERRE & Kahina RAHANI

 **********************************************)

(*************************************
  Fichier : main.ml

  Fichier principal
 *************************************)

(*open Compresseur;
 open Decompresseur;;*)


let md5sum fichier = Digest.to_hex (Digest.file fichier);;

let usage prog =
  print_string "usage : \n";
  print_string (""^prog^" [-c|x] [nom_fichier] \n");
  print_string (""^prog^" [-c|x] -v [nom_fichier] \n");;


let explode_octet s =
  let rec expl i l =
    if i < 0 
    then 
      l 
    else
      expl (i - 1) ((int_of_char(s.[i])):: l) 
  in
  expl (String.length s - 1) [];;


let () = 
  let argc = Array.length (Sys.argv) in
  if argc < 2
  then
    usage Sys.argv.(0)
  else
    print_string "OK";;

