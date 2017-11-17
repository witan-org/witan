(** Need header *)

type t
(** The type of solvers. *)

val mk : (module Theory.S) list -> t
(** Create a solver from a list of theories. *)

