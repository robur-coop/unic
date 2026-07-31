How unic reacts with `digestif.{c,ocaml}`

  $ mkdir -p hash
  $ cat > hash/main.ml <<EOF
  > let () = print_string Digestif.SHA1.(to_hex (digest_string ""))
  > EOF

  $ echo 0 | unic infer hash/
  Module Digestif is provided by several ocamlfind packages:
    [0] digestif.c
    [1] digestif.ocaml
  Pick one [0-1]: digestif

  $ echo 1 | unic infer hash/
  Module Digestif is provided by several ocamlfind packages:
    [0] digestif.c
    [1] digestif.ocaml
  Pick one [0-1]: 

  $ unic infer hash/ < /dev/null
  Module Digestif is provided by several ocamlfind packages:
    [0] digestif.c
    [1] digestif.ocaml
  Pick one [0-1]: unic.exe: no answer was given to choose which package provides Digestif
  [1]

  $ unic infer --prefer digestif.c hash/
  digestif

  $ unic infer --prefer Digestif:digestif.c hash/
  digestif

  $ echo 0 | unic infer --prefer checkseum.c hash/
  Module Digestif is provided by several ocamlfind packages:
    [0] digestif.c
    [1] digestif.ocaml
  Pick one [0-1]: digestif

  $ mkdir -p mix
  $ cat > mix/main.ml <<EOF
  > let () =
  >   let hash = Digestif.SHA1.(to_hex (digest_string "")) in
  >   let crc = Checkseum.Crc32.default in
  >   ignore crc; print_string hash
  > EOF
  $ cat > policy.cfg <<EOF
  > module Digestif {
  >   use digestif.c
  > }
  > 
  > prefer checkseum.c
  > EOF
  $ unic infer --config policy.cfg mix/
  checkseum
  digestif

  $ unic infer --config policy.cfg --prefer Digestif:digestif.ocaml mix/
  checkseum

  $ unic infer --config missing.cfg mix/
  Usage: unic infer [--help] [OPTION]… DIRECTORY…
  unic: option --config: missing.cfg does not exist
  [124]
