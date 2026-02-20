module Assoc : sig
  type t = (string * string list) list
end

module Path : sig
  type t = private string list

  val of_string : string -> (t, [> `Msg of string ]) result
  val of_string_exn : string -> t
  val pp : t Fmt.t
end

type t

val pp : t Fmt.t
val parser : Fpath.t -> (t list, [> `Msg of string ]) result

val search :
     roots:Fpath.t list
  -> ?predicates:string list
  -> Path.t
  -> ((Fpath.t * Assoc.t) list, [> `Msg of string ]) result

val ancestors :
     roots:Fpath.t list
  -> ?predicates:string list
  -> Path.t
  -> ((Path.t * Fpath.t * Assoc.t) list, [> `Msg of string ]) result

val to_artifacts :
  (Fpath.t * Assoc.t) list -> (Uniq_info.t list, [> `Msg of string ]) result

val packages_of_artifacts :
     roots:Fpath.t list
  -> ?predicates:string list
  -> Fpath.t list
  -> (Path.t * Fpath.t) list
(** [packages_of_artifacts ~roots ~predicates artifacts] retourne la liste
    des paquets META [(meta_pkg_path, meta_dir)] dont les archives correspondent
    à l'un des [artifacts] (.cmx/.cmxa/.cma/.cmo), en cherchant dans [roots].
    [meta_dir] est le répertoire contenant le fichier META.
    Les doublons sont éliminés. *)

val setup : Fpath.t list Cmdliner.Term.t
