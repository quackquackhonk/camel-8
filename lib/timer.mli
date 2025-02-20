type t

val create : float -> ?thresh:float -> t

val update : t -> t

val advance : t -> bool
