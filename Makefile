
# Edition des liens et creation de l'executable
huff_kl : Histogramme.cmo Huffman.cmo bitio.cmo compresseur.cmo main.cmo
	ocamlc -o huff_kl Histogramme.cmo Huffman.cmo bitio.cmo compresseur.cmo main.cmo


#Compilation implantation module Histogramme
Histogramme.cmo : Histogramme.ml Histogramme.cmi
	ocamlc -c Histogramme.ml

# Compilation interface module Histogramme
Histogramme.cmi : Histogramme.mli
	ocamlc -c Histogramme.mli 

#Compilation implantation module Huffman
Huffman.cmo :  Huffman.ml  Huffman.cmi
	ocamlc -c  Huffman.ml

# Compilation interface module Huffman
Huffman.cmi :  Huffman.mli
	ocamlc -c  Huffman.mli 

#Compilation implantation module bitio
bitio.cmo :  bitio.ml  bitio.cmi
	ocamlc -c  bitio.ml

# Compilation interface module bitio
bitio.cmi :  bitio.mli
	ocamlc -c bitio.mli 

#Compilation implantation module compresseur
compresseur.cmo :  compresseur.ml  compresseur.cmi
	ocamlc -c  compresseur.ml

# Compilation interface module compresseur
compresseur.cmi :  compresseur.mli
	ocamlc -c compresseur.mli 


# Compilation implantation module main
main.cmo : main.ml main.cmi
	ocamlc -c main.ml

# Compilation interface module main
main.cmi : main.mli
	ocamlc -c main.mli

clean:
	rm -rf huff-kl *.cmi *.cmo *~
