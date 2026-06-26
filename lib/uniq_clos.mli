module Info = Uniq_info
module Meta = Uniq_meta

type env
type disambiguate = Modname.t -> Meta.Path.t list -> Meta.Path.t
type intf = Modname.t * Info.t
type impl = Modname.t * Info.t

val env : ?cfg:Uniq_cfg.t -> Fpath.t list -> (env, [> `Msg of string ]) result
val gamma : env -> Info.t Fpath.Map.t
val stdlib : env -> Fpath.t option

val impls :
     env:env
  -> disambiguate:disambiguate
  -> Info.t list
  -> (Info.t list, [> `Msg of string ]) result

val verify :
     env:env
  -> disambiguate:disambiguate
  -> Info.t list
  -> (intf list * impl list, [> `Msg of string ]) result
