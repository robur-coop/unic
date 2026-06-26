module Info = Uniq_info
module MSet : Set.S with type elt = Modname.t

type cfg
type providers = ?crc:Uniq_digest.t -> Modname.t -> Uniq_info.t option
type private_module = Modname.t * Uniq_digest.t option
type disambiguate = Modname.t -> Uniq_info.t list -> Uniq_info.t

val config :
     ?stdlib:bool
  -> ?recurse:bool
  -> ?exclude:Fpath.t list
  -> ?ignore:Modname.t list
  -> ?forbid:Modname.t list
  -> unit
  -> cfg

val to_ignore : cfg:cfg -> Modname.t -> bool

val solve_intfs :
     ?disambiguate:disambiguate
  -> cfg:cfg
  -> providers:providers
  -> Fpath.t list
  -> (Uniq_info.t list * private_module list, [> `Msg of string ]) result
