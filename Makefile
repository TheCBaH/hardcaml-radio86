OPAM_EXEC = opam exec --
DUNE = $(OPAM_EXEC) dune
RETROCPU ?= .retrocpu
FETCH = scripts/retrocpu-fetch.sh
RIVET_TOOL = scripts/rivet-tool.sh
SEEDS ?= 24
UNITS ?= 150

.PHONY: deps versions build test test-observatory test-vm80a test-local test-workloads workloads-vm80a fmt fmt-check fetch observe-test rtl-test lockstep-test rtl-lockstep-test pin-check ci clean

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

# The math workloads (OCaml oracles, ports, fixtures) in Rivet's model; on the Hardcaml core they are in test-local.
test-workloads:
	$(DUNE) build @workloads/runtest

# The same images on the released vm80a_sim: final RAM and T-states equal the model's.
workloads-vm80a: build
	thirdparty/vm80a/scripts/workloads.sh "$$($(FETCH) $(RETROCPU))"

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

# The same seeded programs on the Hardcaml core, in lockstep with the model.
rtl-lockstep-test: build
	local/scripts/lockstep-programs.sh "$$($(FETCH) $(RETROCPU))" "$$($(RIVET_TOOL))" $(SEEDS) $(UNITS)
	local/scripts/lockstep-programs.sh "$$($(FETCH) $(RETROCPU))" "$$($(RIVET_TOOL))" $(SEEDS) $(UNITS) ult

# The Rivet commit is named in four places.
pin-check:
	scripts/pin-check.sh

ci: fmt-check build test pin-check workloads-vm80a rtl-test observe-test lockstep-test rtl-lockstep-test

clean:
	$(DUNE) clean
