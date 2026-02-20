val setup : unit -> unit
(** [setup ()] initialise la configuration opam (OpamFormatConfig, OpamCoreConfig,
    OpamStateConfig). Doit être appelé avant tout chargement d'état opam. *)

val opam_packages_of_meta_dirs :
     switch_state:'a OpamStateTypes.switch_state
  -> Fpath.t list
  -> OpamPackage.Name.Set.t
(** [opam_packages_of_meta_dirs ~switch_state meta_dirs] retourne l'ensemble
    des noms de paquets opam dont le répertoire d'installation de bibliothèques
    ($prefix/lib/<name>/) est un préfixe de l'un des [meta_dirs] fournis. *)
