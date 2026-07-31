Simple test about unic info.

  $ cat > foo.mli <<EOF
  > type t = int
  > val v : t
  > EOF
  $ cat > foo.ml <<EOF
  > type t = int
  > let v = 42
  > EOF
  $ ocamlfind ocamlopt -c foo.mli foo.ml 2> /dev/null
  $ unic info show foo.cmi | ./normalize.exe --digest --magic
  File: foo.cmi
  Name: Foo
  Version: <version>
  Interfaces imported:
  	<digest>	CamlinternalFormatBasics
  	<digest>	Foo
  	<digest>	Stdlib
  Export:
  	<digest>	Foo
  Missing interfaces:
  	<digest>	CamlinternalFormatBasics
  	<digest>	Stdlib

`unic info search` looks for the objects which provide a given module.

  $ unic info search Foo -I . | sort
  ./foo.cmi
  ./foo.cmx
  ./foo.ml
  ./foo.mli
