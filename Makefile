OPAM_EXEC = opam exec --
DUNE = $(OPAM_EXEC) dune
RETROCPU ?= .retrocpu

.PHONY: deps versions build test fmt fmt-check fetch observe-test ci clean

deps:
	opam install . --deps-only --with-test -y

versions:
	$(OPAM_EXEC) ocamlfind ocamlopt -version
	$(DUNE) --version
	opam list --columns=name,version,pin hwfrp rivet ovcd fst unlz4 err_trace jsont ocamlformat

build:
	$(DUNE) build

test:
	$(DUNE) build @runtest

fmt:
	$(DUNE) build @fmt --auto-promote

fmt-check:
	$(DUNE) build @fmt

# The pinned retrocpu-tools release (vm80a_sim and the exercisers), verified by SHA-256.
fetch:
	scripts/retrocpu-fetch.sh $(RETROCPU)

# Runs the released vm80a_sim and observes its FSTs.
observe-test: build
	scripts/observe-test.sh "$$(scripts/retrocpu-fetch.sh $(RETROCPU))"

ci: fmt-check build test observe-test

clean:
	$(DUNE) clean
