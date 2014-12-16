
(* Type définissant l'arbre de Huffman *)
type arbreH = Feuille of Histogramme.histo | Node of int * arbreH * arbreH


(* Insert un arbre dans une forêt *)
val insert_tree : arbreH -> arbreH list -> arbreH list


(* Exception traitant le cas d'un fichier vide > pas d'histogramme *)
exception Histo_vide


(* Fusionne deux arbres pour n'en former qu'un seul *)
val fusion : arbreH -> arbreH -> arbreH


(* Convertit la liste de couple octet/occurrences en forêt *)
val conversion_histoToArbre : Histogramme.histo list -> arbreH list


(* Construit l'arbre de Huffman à partir d'une forêt *)
val construire_Huffman : arbreH list -> arbreH


(* Définit le type Code comme étant un couple octet/code compressé *)
type codeCompress = Code of (int * int list)


(* Construit le code compressé via le parcours de l'arbre de Huffman *)
val construireCode : arbreH -> int list array -> unit


