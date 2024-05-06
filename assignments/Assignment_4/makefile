.SILENT:
make:
	ocamlc -c ast.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c main.ml
	ocamlc -o out ast.cmo parser.cmo lexer.cmo main.cmo
	./out

clean:
	rm -f lexer.ml parser.ml parser.mli
	rm -f *.c*
	rm out
