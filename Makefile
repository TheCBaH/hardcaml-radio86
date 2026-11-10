OPAM_EXEC = opam exec --
DUNE = $(OPAM_EXEC) dune
RETROCPU ?= .retrocpu
FETCH = thirdparty/vm80a/scripts/retrocpu-fetch.sh
RIVET_TOOL = thirdparty/vm80a/scripts/rivet-tool.sh
SEEDS ?= 24
UNITS ?= 150

.PHONY: deps versions build test test-observatory test-vm80a test-local fmt fmt-check fetch observe-test rtl-test lockstep-test pin-check ci clean

deps:
	opam install . --deps-only --with-test -y

versions:
	$(OPAM_EXEC) ocamlfind ocamlopt -version
	$(DUNE) --version
	$(OPAM_EXEC) ocamlfind list 2>/dev/null | grep -E '^(hwfrp|hardcaml|rivet|ovcd|fst|unlz4|err_trace|jsont|ocamlformat)[. ]' || true

build:
	$(DUNE) build

test:
	$(DUNE) build @runtest

# The unit tests of one part: the common observatory, the third-party RTL, the locally generated RTL.
test-observatory:
	$(DUNE) build @observatory/runtest

test-vm80a:
	$(DUNE) build @thirdparty/runtest

test-local:
	$(DUNE) build @local/runtest

fmt:
	$(DUNE) build @fmt --auto-promote

fmt-check:
	$(DUNE) build @fmt

# The pinned retrocpu-tools release (vm80a_sim and the exercisers), verified by SHA-256.
fetch:
	$(FETCH) $(RETROCPU)

# Runs the released vm80a_sim and observes its FSTs.
observe-test: build
	thirdparty/vm80a/scripts/observe-test.sh "$$($(FETCH) $(RETROCPU))"

# The Hardcaml core in Cyclesim on the fixtures, read back with hwfrp.
rtl-test: build
	local/scripts/rtl-test.sh "$$($(FETCH) $(RETROCPU))"

# Rivet's seeded programs (i8080_tool built from the pinned Rivet commit) under vm80a_sim,
# in lockstep with the model; a planted model bug must be caught.
lockstep-test: build
	thirdparty/vm80a/scripts/lockstep-programs.sh "$$($(FETCH) $(RETROCPU))" "$$($(RIVET_TOOL))" $(SEEDS) $(UNITS)
	thirdparty/vm80a/scripts/lockstep-programs.sh "$$($(FETCH) $(RETROCPU))" "$$($(RIVET_TOOL))" $(SEEDS) $(UNITS) ult

# The Rivet commit is named in four places.
pin-check:
	scripts/pin-check.sh

ci: fmt-check build test pin-check rtl-test observe-test lockstep-test

clean:
	$(DUNE) clean
