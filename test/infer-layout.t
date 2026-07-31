Basic layout of a project (like an unikernel)

  $ mkdir -p proj/lib proj/bin
  $ cat > proj/lib/fixture_core.ml <<EOF
  > let crc = Checkseum.Crc32.default
  > EOF
  $ cat > proj/bin/fixture_cli.ml <<EOF
  > let () = print_string Digestif.SHA1.(to_hex (digest_string ""))
  > EOF
  $ cat > proj/main.ml <<EOF
  > let () = Format.printf "%lx\n%!" (Fixture_core.crc :> int32)
  > EOF

  $ unic infer proj/
  unic: the dependency graph could not be closed:
  missing interfaces:
    Fixture_core (needed by $TESTCASE_ROOT/proj/main.ml)
  [1]

  $ unic infer --recurse --prefer checkseum.c --prefer digestif.c proj/
  checkseum
  digestif

  $ unic infer --recurse --exclude proj/bin/ --prefer checkseum.c proj/
  checkseum

  $ unic infer --recurse --exclude proj/bin/fixture_cli.ml --prefer checkseum.c proj/
  checkseum

  $ mkdir -p gen
  $ cat > gen/main.ml <<EOF
  > let () = ignore (Documents.read "index.html")
  > let () = ignore (Checkseum.Crc32.default)
  > EOF
  $ unic infer --prefer checkseum.c gen/
  unic: the dependency graph could not be closed:
  missing interfaces:
    Documents (needed by $TESTCASE_ROOT/gen/main.ml)
  [1]

  $ unic infer --ignore Documents --prefer checkseum.c gen/
  checkseum

  $ unic infer --ignore Documents,Unused --prefer checkseum.c gen/
  checkseum

  $ unic infer --forbid Checkseum --prefer checkseum.c gen/ --ignore Documents
  unic: the project requires forbidden module(s): Checkseum
  [1]
