
all:

	ocamlc -c Histogramme.ml
	ocamlc -c Huffman.ml
	ocamlc -c bitio.ml
	ocamlc -c compresseur.ml
	ocamlc -o huff-kl Histogramme.cmo Huffman.cmo  bitio.cmo compresseur.cmo

clean:
	rm -rf huff-kl *.cmi *.cmo *~
