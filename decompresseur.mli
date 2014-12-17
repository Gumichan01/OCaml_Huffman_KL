(* Type option pour la lecture du fichier *)
type 'a option = Some of 'a | None


(* Créer la liste des octets 'magiques' *)
val creer_liste_magique : Bitio.bit_in_channel -> int list


(* Compare deux liste via leur taille et leur contenu *)
val comparaison : 'a list -> 'a list -> bool


(* Construit l'arbre de Huffman à partir des infos sur le fichier cible *)
val construire_arbreH : Bitio.bit_in_channel -> Huffman.arbreH


(* Ecrit les données extrait dans le fichier de sortie *)
val ecrire_dans_fichier :
  Bitio.bit_in_channel -> Huffman.arbreH -> out_channel -> 'a


(* Ignore les octets qui ne seront pas utilisés si besoin est *)
val ignorer_octets : Bitio.bit_in_channel -> int -> unit


(* La fonction principale de décompression*)
val decompression : string -> unit
