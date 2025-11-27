(ox (
  (full_repo
   git+https://github.com/oxcaml/opam-repository.git#fec0e1ab7a249b11dfb0786baa0f1ab4502f1796)
  (single_file_http
   https://raw.githubusercontent.com/oxcaml/opam-repository/fec0e1ab7a249b11dfb0786baa0f1ab4502f1796/)))

(dune-overlays (
  (full_repo
   git+https://github.com/dune-universe/opam-overlays.git#d5c12f6d5c7909e6119a82bc4aba682ac3110b2d)
  (single_file_http
   https://raw.githubusercontent.com/dune-universe/opam-overlays/d5c12f6d5c7909e6119a82bc4aba682ac3110b2d/)
  (filter (
    Include (
      astring findlib fmt fpath jsonm logs num ocamlfind uchar uucp xmlm)))))

(opam (
  (full_repo
   git+https://github.com/ocaml/opam-repository.git#e4ede991030ce8363f75a54f15eee4d86e998b5e)
  (single_file_http
   https://raw.githubusercontent.com/ocaml/opam-repository/e4ede991030ce8363f75a54f15eee4d86e998b5e/)))
