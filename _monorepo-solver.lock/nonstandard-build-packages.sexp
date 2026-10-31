((basement v0.18~preview.130.106+341)
 (((sed -ib -e s/caml_state/Caml_state/ src/stubs.c) :if "os = \"macos\"")
  (dune build -p (name) -j (jobs))))

((cmarkit 0.3.0+ox)
 ((
   ocaml
   pkg/pkg.ml
   build
   --dev-pkg
   %{dev}%
   --with-cmdliner
   %{cmdliner:installed}%)))

((dune-rpc 3.20.2+ox)
 (((dune subst) :if dev)
  (rm -rf vendor/csexp)
  (rm -rf vendor/pp)
  (dune build -p (name) -j (jobs) @install (@doc :if with-doc))))

((dyn 3.20.2+ox)
 (((dune subst) :if dev)
  (rm -rf vendor/csexp)
  (rm -rf vendor/pp)
  (dune build -p (name) -j (jobs) @install (@doc :if with-doc))))

((lwt 6.0.0+ox)
 (((dune subst) :if dev)
  (dune exec -p
    (name)
    src/unix/config/discover.exe
    --
    --save
    --use-libev
    %{conf-libev:installed}%)
  (dune build -p
    (name)
    -j
    (jobs)
    @install
    (@runtest :if with-test)
    (@doc     :if with-doc))))

((merlin-lib 5.2.1-502+ox2)
 ((
   sh
   -exc
   "cd external/merlin && dune build -p %{name}% -j %{jobs}% @install && cp %{name}%.install ../.. && ln -sfn external/merlin/_build ../../_build")))

((ocaml-index 1.1+ox2)
 ((
   sh
   -exc
   "cd external/merlin && dune build -p %{name}% -j %{jobs}% @install && cp %{name}%.install ../.. && ln -sfn external/merlin/_build ../../_build")))

((ocamlc-loc 3.20.2+ox)
 (((dune subst) :if dev)
  (rm -rf vendor/csexp)
  (rm -rf vendor/pp)
  (dune build -p (name) -j (jobs) @install (@doc :if with-doc))))

((ordering 3.20.2+ox)
 (((dune subst) :if dev)
  (rm -rf vendor/csexp)
  (rm -rf vendor/pp)
  (dune build -p (name) -j (jobs) @install (@doc :if with-doc))))

((stdune 3.20.2+ox1)
 (((dune subst) :if dev)
  (rm -rf vendor/csexp)
  (rm -rf vendor/pp)
  (dune build -p (name) -j (jobs) @install (@doc :if with-doc))))

((topkg 1.1.1+ox)
 ((ocaml pkg/pkg.ml build --pkg-name (name) --dev-pkg %{dev}%)))

((uutf 1.0.4+ox)
 ((
   ocaml
   pkg/pkg.ml
   build
   --dev-pkg
   %{dev}%
   --with-cmdliner
   %{cmdliner:installed}%)))

((zarith 1.12+ox3)
 (((./configure)
   :if
   "os != \"openbsd\" & os != \"freebsd\" & os != \"macos\"")
  ((sh
    -exc
    "LDFLAGS=\"$LDFLAGS -L/usr/local/lib\" CFLAGS=\"$CFLAGS -I/usr/local/include\" ./configure")
   :if
   "os = \"openbsd\" | os = \"freebsd\"")
  ((sh
    -exc
    "LDFLAGS=\"$LDFLAGS -L/opt/local/lib -L/usr/local/lib\" CFLAGS=\"$CFLAGS -I/opt/local/include -I/usr/local/include\" ./configure")
   :if
   "os = \"macos\" & os-distribution != \"homebrew\"")
  ((sh
    -exc
    "LDFLAGS=\"$LDFLAGS -L/opt/local/lib -L/usr/local/lib\" CFLAGS=\"$CFLAGS -I/opt/local/include -I/usr/local/include\" ./configure")
   :if
   "os = \"macos\" & os-distribution = \"homebrew\" & arch = \"x86_64\"")
  ((sh
    -exc
    "LDFLAGS=\"$LDFLAGS -L/opt/homebrew/lib\" CFLAGS=\"$CFLAGS -I/opt/homebrew/include\" ./configure")
   :if
   "os = \"macos\" & os-distribution = \"homebrew\" & arch = \"arm64\"")
  ((make))))

((dune 3.24.2)
 ((ocaml boot/bootstrap.ml -j (jobs))
  (./_boot/dune.exe
   build
   dune.install
   --release
   --profile
   dune-bootstrap
   -j
   (jobs))))

((react 1.2.2) ((ocaml pkg/pkg.ml build --dev-pkg %{dev}%)))

((uuseg 15.0.0)
 ((
   ocaml
   pkg/pkg.ml
   build
   --dev-pkg
   %{dev}%
   --with-uutf
   %{uutf:installed}%
   --with-cmdliner
   %{cmdliner:installed}%)))
