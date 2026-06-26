module Assoc : sig
  type t = (string * string list) list
end

module Path : sig
  type t = private string list
  (** Type of [ocamlfind] packages (like [foo.bar]). *)

  val of_string : string -> (t, [> `Msg of string ]) result
  val of_string_exn : string -> t
  val pp : t Fmt.t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val parent : t -> t option

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

type t

val pp : t Fmt.t
val parser : Fpath.t -> (t list, [> `Msg of string ]) result

val search :
     roots:Fpath.t list
  -> ?predicates:string list
  -> Path.t
  -> ((Fpath.t * Assoc.t) list, [> `Msg of string ]) result
(** Search the [META] file for the given [path]. *)

val to_artifacts :
  (Fpath.t * Assoc.t) list -> (Uniq_info.t list, [> `Msg of string ]) result
(** Synthesis all artifacts described into the given [META] files into
    {!type:Uniq_info.t} values. *)

val find_providers :
     roots:Fpath.t list
  -> ?predicates:string list
  -> Modname.t list
  -> (Modname.t * Path.t list) list

type archive = Stdlib of Fpath.t | Library of Path.t * Fpath.t * Assoc.t
type package

val packages_with_archive :
  ?predicates:string list -> Fpath.t list -> package list

val from_cmi_to_impl :
     roots:Fpath.t list
  -> packages:package list
  -> ?stdlib:Fpath.t
  -> ?disambiguate:(Modname.t -> Path.t list -> Path.t)
  -> Fpath.t
  -> (archive option, [> `Msg of string ]) result
(** [from_cmi_to_impl ~roots cmi] associates the given [cmi] artifact with the
    [ocamlfind] package that ships it. It returns the package's {!type:Path.t},
    the directory holding its [META] file and its descriptor — a triple whose
    [(directory, descriptor)] projection is directly consumable by
    {!val:to_artifacts} to obtain the implementation archives. It returns [None]
    when no package under [roots] owns the [cmi] (e.g. a project-local unit).

    The association is done by directory first (the [cmi] physically lives in
    its package's [directory]) and falls back to an interface digest comparison
    when the directory is ambiguous or does not match.

    [stdlib], when provided, is the OCaml standard library directory (e.g. from
    the compiler configuration of the chosen toolchain). A [cmi] sitting there
    is associated with the synthetic [stdlib] package, since the standard
    library ships no [META] file. *)

val archives_of :
     roots:Fpath.t list
  -> ?predicates:string list
  -> archive
  -> (Uniq_info.t list, [> `Msg of string ]) result

(**/*)

val ancestors :
     roots:Fpath.t list
  -> ?predicates:string list
  -> Path.t
  -> ((Path.t * Fpath.t * Assoc.t) list, [> `Msg of string ]) result
