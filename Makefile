OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

gen_google_api:
	$(OCAMLBUILD) genapi.byte

test:
	$(OCAMLBUILD) test.byte

clean:
	$(OCAMLBUILD) -clean
