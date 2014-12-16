

(* Definit l'exception du code compressé invalide *)
exception Code_compresse_invalide



(* Permet d'obtenir le code compressé d'un octet donné *)
val getCode : int -> 'a array -> 'a


(* Test si un code compressé est bien binaire *)
val isBit : int list -> bool


(* Met un code compressé dans le fichier cible *)
val injection_bit : Bitio.bit_out_channel -> int list -> unit


(* Lit le fichier et ecrit le code compressé de chaque octet dans la cible *)
val ecrire_code :
  Bitio.bit_out_channel -> int list array -> in_channel -> unit


(* Stocke l'arbre de Huffman dans le fihier cible *)
val stockage_arbre : Bitio.bit_out_channel -> Huffman.arbreH -> unit


(* Effectue la compression *)
val compression : string -> unit


