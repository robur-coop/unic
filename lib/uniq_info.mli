module Path : sig
  type t

  val to_list : t -> Modname.t list
  val of_list : Modname.t list -> t
  val singleton : Modname.t -> t
  val pp : t Fmt.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val is_a_part : part:t -> t -> bool

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

type t = {
    name: Unitname.t
  ; version: int option
  ; exports: (Modname.t * Digest.t option) list
  ; modules: Path.Set.t
  ; intfs: elt list
  ; impls: elt list
  ; format: format
}

and elt =
  | Qualified of Modname.t * Digest.t
  | Fully_qualified of Modname.t * Digest.t * Fpath.t
  | Located of Modname.t * Fpath.t
  | Named of Modname.t

and 'a kind =
  | Ml : Unit.u kind
  | Mli : Unit.u kind
  | Cmo : Cmo_format.compilation_unit kind
  | Cma : Cmo_format.library kind
  | Cmi : Cmi_format.cmi_infos kind
  | Cmx : Cmx_format.unit_infos kind
  | Cmxa : Cmx_format.library_infos kind

and format = Format : 'a kind * 'a -> format

val v : Fpath.t -> (t, [> `Msg of string ]) result
val vs : Fpath.t list -> (t list, [> `Msg of string ]) result
val is_fully_resolved : t -> bool
val is_a_library : t -> bool
val has_c_stubs : t -> bool
val is_native : t -> bool
val is_an_interface : t -> bool
val is_a_cmi : t -> bool
val exports : t -> (Path.t * Digest.t option) list
val crc_of : t -> Modname.t -> Digest.t option
val kind : t -> [ `Intf | `Impl ]
val location : t -> Fpath.t
val modname : t -> Modname.t
val equal : t -> t -> bool
val intfs_imported : t -> (Modname.t * Digest.t option) list
val impls_imported : t -> (Modname.t * Digest.t option) list
val missing : t -> Modname.t list * Modname.t list

val qualify :
  t -> ?location:Fpath.t -> ?crc:Digest.t -> [ `Intf | `Impl ] -> Modname.t -> t

val show : t Fmt.t
val pp : t Fmt.t

(**/**)

val from_object :
     Fpath.t
  -> Misc.Magic_number.info
  -> in_channel
  -> (t, [ `Msg of string ]) result

val to_elt : elt list * elt list -> Deps.dep -> elt list * elt list
