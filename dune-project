(lang dune 3.15)

(using ctypes 0.3)

(name mono)

(generate_opam_files true)

(package
 (name mono)
 (allow_empty)
 (depends
  core
  async
  bonsai
  cohttp_static_handler
  xmlm
  zlib
  of_json
  babel
  fzf
  shexp
  hardcaml
  ppx_hardcaml
  hardcaml_waveterm
  expect_test_helpers_async
  decompress
  ppx_jsonaf_conv))
