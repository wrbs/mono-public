(lang dune 3.15)

(using ctypes 0.3)

(name mono)

(generate_opam_files true)

(pin
 (url _pins/zarith.1.12+ox)
 (package
  (name zarith)
  (version 1.12+ox)))

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
  ppxlib_ast
  bonsai_examples
  bonsai_web_test
  shexp
  hardcaml
  bonsai_web_components
  bonsai_web
  ppx_hardcaml
  hardcaml_waveterm
  ppx_quick_test
  expect_test_helpers_async
  decompress
  ppx_jsonaf_conv))
