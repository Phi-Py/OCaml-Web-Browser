MODULES=parse similarity webgrab style main gui saved
OBJECTS=$(MODULES:=.cmo) 
TEST=tests.byte
MAIN=main.byte
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind -pkg graphics
PKGS=ounit2,yojson,graphics,curl,str,csv

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS) $(MAIN)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

gui: build
	./$(MAIN)

zip:
	zip webcaml.zip *.ml* _tags Makefile .merlin .ocamlinit *.md* saved_files/*.csv*

docs: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

clean:
	ocamlbuild -clean
	rm -rf doc.public