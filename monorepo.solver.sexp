(repos (
  (ox ((source (github oxcaml/opam-repository main))))
  (dune-overlays (
    (source (github dune-universe/opam-overlays master))
    ;; only include things explicitly that need the duniverse patch
    ;; (the repo has some old versions of things that switched to dune, but
    ;; because of how repo prioritization works this will cause us to pick up
    ;; old versions)
    (filter (
      include (
        astring
        cmdliner
        ocamlfind
        findlib
        fmt
        fpath
        jsonm
        logs
        num
        uchar
        uucp
        uunf
        xmlm
        uutf
        cmarkit
        seq)))))
  (opam ((source (github ocaml/opam-repository master))))))

(env (
  (arch            x86_64)
  (os              linux)
  (os_distribution linux)
  (os_family       linux)
  (os_version      system)))

(package_selection (
  ;; oxcaml-compiler is the real compiler package; ocaml-variants is a stub
  (include (
    (ocaml           = 5.2.0)
    (ocaml-variants  = 5.2.0+ox)
    (oxcaml-compiler = 5.2.0minus40)))
  (include ((ocaml-lsp-server = 1.19.0+ox2) ocamlformat))
  (include (cinaps))
  (include_all_matching_version
    ;; this is highly specialized for getting latest version of all JS packages
    ;; in ox, probably no other use case
    (in_repo ox)
    (of_     core))
  ;; unnecesary dune-overlays matches (could complicate solver to resolve but
  ;; this works for now)
  (include (
    (astring >= 0.8.5)
    (logs    =  0.10.0+dune)
    (menhir  >  20200624)
    ;; newer uucp isn't in dune-overlays; patches/uuseg.patch is for 15.0.0
    (uucp    =  15.0.0+dune)
    (uuseg   =  15.0.0)))
  ;; not working
  (exclude (netsnmp torch ocaml_simd ppx_demo))
  ;; needs jingoo -> ppx_deriving, which oxcaml-patch-guards forbids for
  ;; oxcaml-compiler > 5.2.0minus25
  (exclude (ocaml_openapi_generator))
  (exclude (
    ;; opam memtrace doesn't work with compiler libs
    ecaml
    memtrace_viewer))
  ;; 6.0 beta too new
  (include (
    (cohttp       = 5.3.0)
    (cohttp-async = 5.3.0)))
  ;; custom for mono
  (include (decompress grace))))

(vendoring (
  (exclude_pkgs (
    ocamlbuild
    ocamlfind
    findlib
    dune-configurator
    ppxlib_ast
    ocamlformat-rpc-lib
    oxcaml-compiler))
  ;; avoid naming conflicts where oxcaml repo splits same source into different
  ;; packages with different patches
  (rename_dirs ())
  ;; use another package's upstream source + patches, for when packages built from
  ;; the same source dir disagree
  (use_source_of (
    ;; ox repo only bumped js_of_ocaml + js_of_ocaml-compiler to a newer commit
    (js_of_ocaml-ppx      js_of_ocaml)
    (js_of_ocaml-toplevel js_of_ocaml)
    ;; same patches in a different order (they don't overlap)
    (js_of_ocaml-compiler js_of_ocaml)
    ;; ocaml-lsp-server+ox2 needs stdune < 3.22, and on oxcaml-compiler minus40
    ;; the only allowed one is 3.20.2+ox1 (its patch is a superset of +ox's).
    ;; The rest of the dune tree resolves to assorted versions, but it's one
    ;; source dir so build it all from stdune's source
    (chrome-trace    stdune)
    (dune            stdune)
    (dune-build-info stdune)
    (dune-rpc        stdune)
    (dyn             stdune)
    (ocamlc-loc      stdune)
    (ordering        stdune)
    (xdg             stdune)))
  ;; replace a package's upstream source (keeping its own patches)
  (source_overrides (
    ;; merlin-lib/ocaml-index+ox2 are built from external/merlin of a post-minus39
    ;; oxcaml commit, which can't read minus40's .cmi files: use the minus40 tag
    ;; (same tarball as oxcaml-compiler)
    (merlin-lib
     ((urls (https://github.com/oxcaml/oxcaml/archive/refs/tags/5.2.0minus-40.tar.gz))
      (hashes (
        sha256=c051836f1c84dc359dc0bca4590c2e93a8a609094d13c8edcdb0ec579caced11))))
    (ocaml-index
     ((urls (https://github.com/oxcaml/oxcaml/archive/refs/tags/5.2.0minus-40.tar.gz))
      (hashes (
        sha256=c051836f1c84dc359dc0bca4590c2e93a8a609094d13c8edcdb0ec579caced11))))))
  ;; exclude source dirs, no matter what packages provided them
  (exclude_dirs (ocaml bytes))
  ;; custom commands in the opam file that need to run before the build
  (prepare_commands (
   ;; as the dune-family opam build steps do: use the standalone csexp/pp
   ;; rather than dune's vendored copies (same module names)
   (dune_ ((rm -rf vendor/csexp vendor/pp)))
   (merlin (
      ;; merlin-lib/ocaml-index+ox2 are built from external/merlin in the
      ;; oxcaml compiler source: keep only that
      (bash
       -O
       extglob
       -O
       dotglob
       -c
       "rm -rf !(external) && mv external/merlin/* . && rm -rf external")
      (rm
      -f
      src/ocaml/preprocess/parser_raw.ml
      src/ocaml/preprocess/parser_raw.mli
      src/ocaml/preprocess/parser_recover.ml
      src/ocaml/preprocess/parser_explain.ml
      src/ocaml/preprocess/parser_printer.ml
      src/ocaml/preprocess/menhirLib.ml
      src/ocaml/preprocess/menhirLib.mli
      src/sherlodoc/type_parser.ml
      src/sherlodoc/type_parser.mli)))))))
