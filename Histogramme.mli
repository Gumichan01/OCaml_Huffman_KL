
(* Le type histogramme, une paire octet-nombre d'occurrences *)
type histo = Couple of (int * int)


(* Met à jour le nombre d'occurrences d'un octet présent dans le fichier *)
val succ_histo : int -> int array -> unit


(* Insert un type histo dans une liste existante *)
val insert_histo : histo -> histo list -> histo list


(* Tri l'histogramme *)
val sort_histo : histo list -> histo list


(* Défini un type option pour la gestion de fin de fichier en lecture/écriture *)
type 'a option = Some of 'a | None


(* Lit le fichier et créer l'histogramme au fur et à mesure de la lecture *)
val creer_histo : in_channel -> int array


(* Convertit le tableau génèré par la fonction creer_histo en liste *)
val convertir : int array -> histo list


