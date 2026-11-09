OPAM_EXEC = opam exec --
DUNE = $(OPAM_EXEC) dune
RETROCPU ?= .retrocpu
SEEDS ?= 24
UNITS ?= 150

.PHONY: deps versions build test fmt fmt-check fetch observe-test lockstep-test pin-check ci clean

deps:
	opam install . --deps-only --with-test -y

versions:
	$(OPAM_EXEC) ocamlfind ocamlopt -version
	$(DUNE) --version
	$(OPAM_EXEC) ocamlfind list 2>/dev/null | grep -E '^(hwfrp|rivet|ovcd|fst|unlz4|err_trace|jsont|ocamlformat)[. ]' || true

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

# Rivet's seeded programs (i8080_tool built from the pinned Rivet commit) under vm80a_sim,
# in lockstep with the model; a planted model bug must be caught.
lockstep-test: build
	scripts/lockstep-programs.sh "$$(scripts/retrocpu-fetch.sh $(RETROCPU))" "$$(scripts/rivet-tool.sh)" $(SEEDS) $(UNITS)
	scripts/lockstep-programs.sh "$$(scripts/retrocpu-fetch.sh $(RETROCPU))" "$$(scripts/rivet-tool.sh)" $(SEEDS) $(UNITS) ult

# The Rivet commit is named in four places.
pin-check:
	scripts/pin-check.sh

ci: fmt-check build test pin-check observe-test lockstep-test

clean:
	$(DUNE) clean
