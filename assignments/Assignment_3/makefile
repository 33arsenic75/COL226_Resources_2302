.SILENT:

all: 
	ocamlc -c ast.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c token.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -o output ast.cmo lexer.cmo token.cmo parser.cmo main.ml 
	./output
clean:
	rm -f parser.ml parser.mli parser.cmi parser.cmo ast.cmi ast.cmo lexer.cmi lexer.cmo lexer.ml main.cmi main.cmo token.cmo token.cmi output