
# Edition des liens et creation de l'executable
huff-kl : Histogramme.cmx Huffman.cmx bitio.cmx compresseur.cmx decompresseur.cmx main.cmx
	ocamlopt -o huff-kl Histogramme.cmx Huffman.cmx bitio.cmx compresseur.cmx decompresseur.cmx main.cmx


# Bibliothèque Histogramme
#Compilation implantation module Histogramme
Histogramme.cmx : Histogramme.ml Histogramme.cmi
	ocamlopt -c Histogramme.ml

# Compilation interface module Histogramme
Histogramme.cmi : Histogramme.mli
	ocamlopt -c Histogramme.mli 


# Bibliothèque Huffman
#Compilation implantation module Huffman
Huffman.cmx :  Huffman.ml  Huffman.cmi
	ocamlopt -c Huffman.ml

# Compilation interface module Huffman
Huffman.cmi :  Huffman.mli
	ocamlopt -c Huffman.mli 


# Bibliothèque bitio
#Compilation implantation module bitio
bitio.cmx :  bitio.ml  bitio.cmi
	ocamlopt -c bitio.ml

# Compilation interface module bitio
bitio.cmi :  bitio.mli
	ocamlopt -c bitio.mli 

#Compilation implantation module compresseur
compresseur.cmx :  compresseur.ml  compresseur.cmi
	ocamlopt -c compresseur.ml


# Bibliothèque compresseur
# Compilation interface module compresseur
compresseur.cmi :  compresseur.mli
	ocamlopt -c compresseur.mli 


# Bibliothèque decompresseur
# Compilation implantation module decompresseur
decompresseur.cmx : decompresseur.ml decompresseur.cmi
	ocamlopt -c decompresseur.ml

# Compilation interface module decompresseur
decompresseur.cmi : decompresseur.mli
	ocamlopt -c decompresseur.mli

# Le Main du programme
# Compilation implantation module main
main.cmx : main.ml main.cmi
	ocamlopt -c main.ml

# Compilation interface module main
main.cmi : main.mli
	ocamlopt -c main.mli

clean:
	rm -rf *.cmi *.cmx *~ *.o

cleanall:
	rm -rf huff-kl *.cmi *.cmx *~ *.o

