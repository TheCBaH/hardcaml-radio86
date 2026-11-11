(lang dune 3.21)

(name hardcaml-radio86)

(generate_opam_files true)

(license MIT)

(authors "Vladimir Silyaev")

(maintainers "Vladimir Silyaev")

(source
 (github TheCBaH/hardcaml-radio86))

(package
 (name hardcaml-radio86)
 (synopsis "Observation of an Intel 8080 from waveform traces")
 (description
  "Hardcaml i8080 CPU and its harness; reads Verilator or Cyclesim waveforms of an 8080 with hwfrp and decodes bus transfers, instructions and register state, checked in lockstep against Rivet's behavioural model.")
 (depends
  (ocaml
   (>= 4.14))
  hwfrp
  hardcaml
  ppx_hardcaml
  rivet
  jsont
  bytesrw
  cmdliner
  (ppx_expect :with-test)
  (qcheck-core :with-test)))
