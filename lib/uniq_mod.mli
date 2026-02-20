type k = [ `All | `Intf | `Impl ]
type t = [ `All | `Sources | `Objects ]
type c = [ `All | `Bytecode | `Native ]

val search :
     ?filters:k * t * c
  -> roots:Fpath.t list
  -> Uniq_info.Path.t
  -> Uniq_digest.t option
  -> ((Fpath.t * Uniq_info.t) list, [> `Msg of string ]) result
(** [search ?filters ~roots p digest] searches for an OCaml object according to
    a module path and an optional signature. The result can be filtered by the
    type of object using the [filters] option. We only searches in the folders
    specified by [roots]. *)
