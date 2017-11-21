(****************************************************)
(* This module is to be opened before anything else *)
(* as it redefines OCaml's standard stuff           *)
(****************************************************)

val (=) : int -> int -> bool
val (<>) : int -> int -> bool
val (<) : int -> int -> bool
val (>) : int -> int -> bool
val (<=) : int -> int -> bool
val (>=) : int -> int -> bool
val compare : int -> int -> int
val max: compare:('a->'a->int) -> 'a -> 'a -> 'a
val min: compare:('a->'a->int) -> 'a -> 'a -> 'a
val lex_compare: ('a->'a->int) -> ('b->'b->int) -> ('a*'b) -> ('a*'b) -> int
val show_of_pp: (Format.formatter -> 'a -> unit) -> 'a -> string
  
module Pervasives : sig
  val compare : int -> int -> int
  val max  : int -> int -> int
  val min  : int -> int -> int
  val exit : int -> 'a
end

module List : sig
  type 'a t = 'a list [@@deriving eq]
  val pp : ?sep:string -> ?wrap:string*string
           -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val mem : equal:('a -> 'a -> bool) -> 'a -> 'a list -> bool
  val map : ('a -> 'b) -> 'a list -> 'b list
  val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val for_all : ('a -> bool) -> 'a list -> bool
  val rev : 'a list -> 'a list
  val length : 'a list -> int
  val hd : 'a list -> 'a
  val tl : 'a list -> 'a list
  val append : 'a list -> 'a list -> 'a list
  val rev_append : 'a list -> 'a list -> 'a list
  val filter : ('a -> bool) -> 'a list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val iteri: (int -> 'a -> unit) -> 'a list -> unit
  val sort : ('a -> 'a -> int) -> 'a list -> 'a list
end