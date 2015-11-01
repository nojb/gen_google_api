OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

all:
	cat genapi.ml | sed 's/!!SECRET!!/$(shell cat SECRET)/g' > genapi2.ml
	$(OCAMLBUILD) genapi2.byte

clean:
	$(OCAMLBUILD) -clean
