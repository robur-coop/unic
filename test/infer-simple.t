Basic usage of `unic`

  $ mkdir -p pure
  $ cat > pure/pure_main.ml <<EOF
  > let () = Fmt.pr "%d\n%!" 42
  > EOF
  $ unic infer pure/

`bstr` is taken as a library with a C stub

  $ mkdir -p stubs
  $ cat > stubs/stubs_main.ml <<EOF
  > let () = ignore (Bstr.create 42)
  > EOF
  $ unic infer stubs/
  bstr

Check transitive dependencies

  $ mkdir -p transitive
  $ cat > transitive/transitive_main.ml <<EOF
  > let window = De.Lz77.make_window ~bits:15
  > EOF
  $ unic infer --prefer checkseum.c transitive/
  checkseum
  decompress

  $ unic infer --prefer checkseum.c pure/ stubs/ transitive/
  bstr
  checkseum
  decompress

  $ unic infer
  Usage: unic infer [--help] [OPTION]… DIRECTORY…
  unic: required argument DIRECTORY is missing
  [124]
