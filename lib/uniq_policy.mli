type t
type path = Uniq_meta.Path.t

val empty : t
val load : Fpath.t -> (t, [> `Msg of string | Bcfg.error ]) result
val disambiguate_with : t -> Modname.t -> path list -> path option
val use : t -> Modname.t -> path -> t
val prefer : t -> path -> t
