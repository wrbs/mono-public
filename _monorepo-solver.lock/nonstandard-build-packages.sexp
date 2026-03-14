((chrome-trace 3.21.0+ox)
 (((dune subst) :if dev)
  (rm -rf vendor/csexp)
  (rm -rf vendor/pp)
  (dune build -p (name) -j (jobs) @install (@doc :if with-doc))))

((dune 3.21.0+ox)
 ((ocaml boot/bootstrap.ml -j (jobs))
  (./_boot/dune.exe
   build
   dune.install
   --release
   --profile
   dune-bootstrap
   -j
   (jobs))))

((dune-build-info 3.21.0+ox)
 (((dune subst) :if dev)
  (rm -rf vendor/csexp)
  (rm -rf vendor/pp)
  (dune build -p (name) -j (jobs) @install (@doc :if with-doc))))

((dune-rpc 3.21.0+ox)
 (((dune subst) :if dev)
  (rm -rf vendor/csexp)
  (rm -rf vendor/pp)
  (dune build -p (name) -j (jobs) @install (@doc :if with-doc))))

((dyn 3.21.0+ox)
 (((dune subst) :if dev)
  (rm -rf vendor/csexp)
  (rm -rf vendor/pp)
  (dune build -p (name) -j (jobs) @install (@doc :if with-doc))))

((fs-io 3.21.0+ox)
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

((ocamlc-loc 3.21.0+ox)
 (((dune subst) :if dev)
  (rm -rf vendor/csexp)
  (rm -rf vendor/pp)
  (dune build -p (name) -j (jobs) @install (@doc :if with-doc))))

((ordering 3.21.0+ox)
 (((dune subst) :if dev)
  (rm -rf vendor/csexp)
  (rm -rf vendor/pp)
  (dune build -p (name) -j (jobs) @install (@doc :if with-doc))))

((stdune 3.21.0+ox)
 (((dune subst) :if dev)
  (rm -rf vendor/csexp)
  (rm -rf vendor/pp)
  (dune build -p (name) -j (jobs) @install (@doc :if with-doc))))

((top-closure 3.21.0+ox)
 (((dune subst) :if dev)
  (rm -rf vendor/csexp)
  (rm -rf vendor/pp)
  (dune build -p (name) -j (jobs) @install (@doc :if with-doc))))

((topkg 1.0.8+ox)
 ((ocaml pkg/pkg.ml build --pkg-name (name) --dev-pkg %{dev}%)))

((uutf 1.0.3+ox)
 ((
   ocaml
   pkg/pkg.ml
   build
   --dev-pkg
   %{dev}%
   --with-cmdliner
   %{cmdliner:installed}%)))

((xdg 3.21.0+ox)
 (((dune subst) :if dev)
  (rm -rf vendor/csexp)
  (rm -rf vendor/pp)
  (dune build -p (name) -j (jobs) @install (@doc :if with-doc))))

((zarith 1.12+ox)
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

((cmdliner 1.3.0) (((make) all PREFIX=%{prefix}%)))

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
