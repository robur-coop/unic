type _ Effect.t +=
  | Read_file : Read.kind * string * Namespaced.t -> Unit.s Effect.t

exception Invalid_source_file of string

val run :
     ?version:Sys.ocaml_release_info
  -> ?stdlib:bool
  -> Name.t list
  -> Unit.u list Unit.pair

val run_into :
     ?version:Sys.ocaml_release_info
  -> ?stdlib:bool
  -> current:Fpath.t
  -> Name.t list
  -> Unit.u list Unit.pair
(** [run_into ?version ~current filenames] it resolves the dependencies of given
    source files [filenames] in a given folder [current] (considered to be the
    root for the namespace).

    Unlike {!val:run}, this function manages the {!constr:Read_file} effect.

    @raise Invalid_source_file
      if one of the files is not an OCaml file ([.ml], [.mli] or [.cmi]). *)
