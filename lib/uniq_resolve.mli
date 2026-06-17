module Src : sig
  type directory = { recurse: bool; location: Fpath.t }

  type t =
    private
    [ `File of Fpath.t | `Sources of directory | `Objects of directory ]

  val pp : t Fmt.t
  val file : Fpath.t -> t
  val sources : ?recurse:bool -> Fpath.t -> t
  val objects : ?recurse:bool -> Fpath.t -> t
end

val qualify_objects : Uniq_info.t list -> Uniq_info.t list

val qualify :
  ?stdlib:bool -> Src.t list -> (Uniq_info.t list, [> `Msg of string ]) result
