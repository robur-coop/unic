type t

val v : ?env:Bos.OS.Env.t -> unit -> (t, [> `Msg of string ]) result
val from : t -> Fpath.t

module Value : sig
  type _ t

  val string : string t
  val list : ?sep:string -> 'a t -> 'a list t
  val bool : bool t
  val int : int t
  val path : Fpath.t t
end

val get : ?native:bool option -> t -> key:string -> 'a Value.t -> 'a option
val setup : t option Cmdliner.Term.t
