`unic` attempts to infer dependencies from the source code using codept. There
are currently three limitations to this resolution process, along with their
current solutions:

### when two implementations refer to the same `*.cmi`

This is particularly the case with `digestif.{c,ocaml}`. There is no real way to
distinguish between the two, so the user is currently asked to choose, and that
choice will be retained throughout the process.

### when a submodule appears in several `*.cmi` files

This is the most complicated case that could be resolved using `codept`, I
think. It’s a situation where you call `open Cmdliner` and then use the `Term`
module. The problem is that this module may exist within other dependencies, and
only the values (which `codept` handles) can truly confirm that `Term` is indeed
the submodule found in `cmdliner.cmi`. Currently, when loading a new `*.cmi`
based on module names, we also identify any submodules missing from this new
inclusion (we look for `Cmdliner`, load `cmdliner.cmi`, look for `Term`, find it
first in `cmdliner.cmi`, and therefore do not look for it in our environment).

There is therefore a scenario where `Term` is searched for **before**
`Cmdliner`, and several solutions may be found. In this case, the `--prefer`
option allows you to associate a module name with a specific package if `unic`
cannot resolve it.

### when a package exists but does not provide a `*.cmi` file

This is perhaps the most complicated issue and falls outside the scope of
`codept`. There may be `*.cmxa` files that require libraries from specific
directories. These directories may refer to a standalone `ocamlfind` package
(as is the case with `gmp`) but do not produce any `*.cmi` files. We still need
to include them, and we could verify whether they are actually required for
static linking if we were collecting the `external`s, which is not the case.

For the time being, we iterate through these directories and check whether they
might be packages. If they are indeed `ocamlfind`/`opam` packages, we include
them in the list of dependencies to be bundled.

### result

Overall, across several unikernels, we are able to infer everything correctly.

So the issues explained above remain minor, and the chosen heuristics appear to
be sound. However, it is important to bear in mind that `unic` can be fallible
in these cases, and the correct solutions (qualifying sub-modules using values,
finding the correct implementation from among several options, including
archives even where there are no `*.cmi` files) are not as obvious as one might
think.
