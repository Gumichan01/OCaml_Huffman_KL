(**********************************************
        Projet OCaml S5

  Auteurs : Luxon JEAN-PIERRE & Kahina RAHANI

 **********************************************)

(*************************************
  Fichier : main.ml

  Fichier principal
 *************************************)

open Compresseur;;
open Decompresseur;;



let usage prog =
  print_string "usage : \n";
  print_string (""^prog^" [-c|x] [nom_fichier] \n");;



let () = 
  let argc = Array.length (Sys.argv) in
  if (argc < 2 || argc > 3)
  then
    usage Sys.argv.(0)
  else
    begin
      if argc = 2 
      then
	if Sys.argv.(1) <> "-c" && Sys.argv.(1) <> "-d"
	then
	  compression (Sys.argv.(argc - 1))
	else
	  begin
	    print_string (Sys.argv.(0)^" : Je refuse de travailler sur "^Sys.argv.(1)^", même si c'est un fichier ! \n");
	    raise Exit
	  end 
      else
	if Sys.argv.(1) = "-c"
	then 
	  compression (Sys.argv.(argc - 1))
	else
	  if Sys.argv.(1) = "-d"
	  then
	    decompression (Sys.argv.(argc - 1))
	  else
	    begin
	      print_string (Sys.argv.(0)^" : option "^Sys.argv.(1)^" non valide ! \n");
	      usage Sys.argv.(0);
	      raise Exit
	    end
    end
;;

