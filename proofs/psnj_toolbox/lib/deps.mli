module Path : sig
  type t

  val is_predecessor : t -> t -> bool
  (** [is_predecessor p q] is true if [q = p.i] ([p] is closer to the
      root). *)

  val of_string : string -> t
  (** [of_string s] parses a string [s] into a path. The string [s]
      may be ["root"] or a dot-separated list of integers.
      @raise Invalid_argument when [s] is ill-formed *)
end

module Id : sig
  type t

  val make : string -> t
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
end

module IdMap : Map.S with type key = Id.t

type t
(** The type of dependencies collections *)

val get : Id.t -> t -> Id.t list
(** [get id c] returns the dependencies of identifier [id] in collection
    [c]. *)

val of_paths : Path.t IdMap.t -> t
(** [of_paths ps] creates a dependencies collection out of a mapping
    [ps] from identifiers to paths. *)

val pp_makefile : Format.formatter -> t -> unit
(** [pp_makefile ppf c] prints dependencies of [c] in makefile syntax
    on formatter [ppf]. *)

val of_makefile : in_channel -> t
(** [of_makefile ic] parses a makefile dependency list from channel [ic]. *)
