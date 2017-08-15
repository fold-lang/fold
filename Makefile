# findlib: [WARNING] Interface topdirs.cmi occurs in several directories...
OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

OCAMLRUNPARAM = b
export OCAMLRUNPARAM

# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

docker-build:
	@echo "==> [$@] Building development docker image..."
	docker build -t fold -t foldlang/fold .

docker-run:
	@echo "==> [$@] Running docker image..."
	docker run -it -v "${PWD}:/fold" fold bash

docker-push: docker-build
	@echo "==> [$@] Pushing docker image to registry..."
	docker push foldlang/fold

setup:
	cat ~/.ocamlinit init.ml > /tmp/fold_init.ml

live:
	utop -init /tmp/fold_init.ml

watch:
	ls src/*.{ml,mli} tests/*.ml | entr -cr sh -c 'make build && ./Main.byte'

