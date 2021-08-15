MODULES=plumbing porcelain command state renderer ogit authors
OBJECTS=$(MODULES:=.cmo)
BYTES=$(MODULES:=.byte)
MLS_WITHOUT_MLIS=plumbing porcelain ogit renderer test
MLS=$(UNITS:=.ml) $(MLS_WITHOUT_MLIS:=.ml)
MLIS_WITH_MLS=authors command
MLIS=$(UNITS:=.mli) $(MLIS_WITH_MLS:=.mli)
DOCS=command.mli
TEST=test.byte
MAIN=ogit.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=ounit2,curses,str,yojson

default: build

utop: build
		OCAMLRUNPARAM=b utop

build:
		$(OCAMLBUILD) $(BYTES)

test:
		$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

run:
		$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

check:
		@bash check.sh
			
finalcheck:
		@bash check.sh final

zip:
		zip project.zip *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile INSTALL.md
			
clean:
		ocamlbuild -clean
			rm -rf _doc.public

docs: docs-public

docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d _doc.public authors.mli command.mli plumbing.ml porcelain.ml state.ml renderer.ml ogit.ml

