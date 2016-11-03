all: parser
parser:
	ocamlbuild -use-menhir -use-ocamlfind test.native
clean:
	rm -rf _build; rm test.native
tests:
	bash ../test/hotests/hotest.sh; bash ../test/optests/test.sh
