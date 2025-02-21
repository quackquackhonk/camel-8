type t

val create : ?thresh:float -> float -> t

val update : t -> t
(** [update timer] sets the [last_update] field of [timer] to the current time. *)

val advance : t -> int
(** [advance timer] returns the number of cycles needed to be executed since the last update.*)
