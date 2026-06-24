module MSet : Set.S with type elt = Modname.t

module Config : sig
  type t = {
      stdlib: bool
    ; recurse: bool
    ; exclude: Fpath.t list
    ; ignore: MSet.t
    ; forbid: MSet.t
    ; roots: Fpath.t list
    ; policy: Uniq_policy.t
  }

  val cfg :
       ?stdlib:bool
    -> ?recurse:bool
    -> ?exclude:Fpath.t list
    -> ?ignore:Modname.t list
    -> ?forbid:Modname.t list
    -> ?policy:Uniq_policy.t
    -> Fpath.t list
    -> t
end

type disambiguate = Modname.t -> Uniq_meta.Path.t list -> Uniq_meta.Path.t

exception Ambiguous of Modname.t * Uniq_meta.Path.t list

val fail_on_ambiguity : disambiguate

type node = {
    dirpath: Fpath.t
  ; objs: Uniq_info.t list
  ; deps: (Uniq_meta.Path.t * [ `CRC | `Name ]) list
}

type graph = node Uniq_meta.Path.Map.t

val solve :
     cfg:Config.t
  -> ?disambiguate:disambiguate
  -> Fpath.t list
  -> (graph, [> `Msg of string ]) result

module Ng : sig
  type cfg
  type providers = ?crc:Uniq_digest.t -> Modname.t -> Uniq_info.t option

  val config :
       ?stdlib:bool
    -> ?recurse:bool
    -> ?exclude:Fpath.t list
    -> ?forbid:Modname.t list
    -> unit
    -> cfg

  val solve_intfs :
       cfg:cfg
    -> providers:providers
    -> Fpath.t list
    -> ( Uniq_info.t list * (Modname.t * Uniq_digest.t option) list
       , [> `Msg of string ] )
       result
end
