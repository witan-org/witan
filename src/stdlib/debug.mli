(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2017   --   INRIA - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)


type flag

val register_flag : desc:Pp.formatted -> string -> flag
(** Register a new flag. It is allowed to register twice the same flag *)

val register_info_flag : desc:Pp.formatted -> string -> flag
(** Register a new info flag. It is allowed to register twice the same flag.
    Info flags are set by --debug-all and must not change the behaviour. *)

val list_flags : unit -> (string * flag * bool * Pp.formatted) list
(** List the known flags *)

val lookup_flag : string -> flag
(** get the flag *)

val is_info_flag : string -> bool
(** test if the flag is an info flag *)

val flag_desc : string -> Pp.formatted
(** get the description of the flag *)

(** *)
val _true: flag
(** a flag always true; can't be modified  *)

val todo: flag
(** a flag always true; can't be modified
    should be used for temporary debugging message
*)

(** Modify the state of a flag *)
val set_flag : flag -> unit
val unset_flag : flag -> unit
val toggle_flag : flag -> unit

(** Return the state of the flag *)
val test_flag : flag -> bool
val test_noflag : flag -> bool

val set_debug_formatter : Format.formatter -> unit
(** Set the formatter used when printing debug material *)

val get_debug_formatter : unit -> Format.formatter
(** Get the formatter used when printing debug material *)

val dprintf0 : ?nobox:unit -> flag -> (unit,
  Format.formatter, unit) format -> unit
(** Print only if the flag is set *)

val dprintf1 : ?nobox:unit -> flag -> ('a -> unit,
  Format.formatter, unit) format -> 'a -> unit
(** Print only if the flag is set *)

val dprintf2 : ?nobox:unit -> flag -> ('b -> 'a -> unit,
  Format.formatter, unit) format ->
  'b -> 'a -> unit
(** Print only if the flag is set *)

val dprintf3 : ?nobox:unit -> flag -> ('c -> 'b -> 'a -> unit,
  Format.formatter, unit) format ->
  'c -> 'b -> 'a -> unit
(** Print only if the flag is set *)

val dprintf4 : ?nobox:unit -> flag -> ('d -> 'c -> 'b -> 'a -> unit,
  Format.formatter, unit) format ->
  'd -> 'c -> 'b -> 'a -> unit
(** Print only if the flag is set *)

val dprintf5 : ?nobox:unit -> flag -> ('e -> 'd -> 'c -> 'b -> 'a -> unit,
  Format.formatter, unit) format ->
  'e -> 'd -> 'c -> 'b -> 'a -> unit
(** Print only if the flag is set *)

val dprintf6 : ?nobox:unit -> flag ->
  ('f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit,
  Format.formatter, unit) format ->
  'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit
(** Print only if the flag is set *)

val dprintf7 : ?nobox:unit -> flag ->
  ('g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit,
  Format.formatter, unit) format ->
  'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit
(** Print only if the flag is set *)

val dprintf8 : ?nobox:unit -> flag ->
  ('h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit,
  Format.formatter, unit) format ->
  'h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit
(** Print only if the flag is set *)

val dprintf9 : ?nobox:unit -> flag ->
  ('i -> 'h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit,
  Format.formatter, unit) format ->
  'i -> 'h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit
(** Print only if the flag is set *)

val dprintf10 : ?nobox:unit -> flag ->
  ('j -> 'i -> 'h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit,
  Format.formatter, unit) format ->
  'j -> 'i -> 'h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit
(** Print only if the flag is set *)

val dprintf11 : ?nobox:unit -> flag ->
  ('k -> 'j -> 'i -> 'h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit,
  Format.formatter, unit) format ->
  'k -> 'j -> 'i -> 'h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit
(** Print only if the flag is set *)

val dprintf12 : ?nobox:unit -> flag ->
  ('l -> 'k -> 'j -> 'i -> 'h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit,
  Format.formatter, unit) format ->
  'l -> 'k -> 'j -> 'i -> 'h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit
(** Print only if the flag is set *)

val dprintfn : ?nobox:unit -> flag ->
  ('i -> 'h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> 'z,
  Format.formatter, unit) format ->
  'i -> 'h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> 'z
(** Print only if the flag is set *)

(* val dprintf : flag -> ('a, Format.formatter, unit) format -> 'a *)
(** Print only if the flag is set *)

val stack_trace : flag
(** stack_trace flag *)

val noassert : bool
(** is -noassert set *)

(** Command line arguments *)
module Args : sig
  type spec = (Arg.key * Arg.spec * Arg.doc)

  val desc_debug_list : spec
  (** Option for printing the list of debug flags *)

  val option_list : unit -> bool
  (** Print the list of flags if requested (in this case return [true]).
      You should run this function after the plugins have been loaded. *)

  val desc_debug_all : spec
  (** Option for setting all info flags *)

  val desc_debug : spec list
  (** Option for specifying a debug flag to set *)

  val desc_shortcut : string -> Arg.key -> Arg.doc -> spec
  (** Option for setting a specific flag *)

  val set_flags_selected : unit -> unit
  (** Set the flags selected by debug_all, debug or a shortcut.
      You should run this function after the plugins have been loaded. *)
end

(** Stats *)
val stats: flag

type 'a stat

val register_stats :
  pp:('a Pp.pp) ->
  name:string ->
  init:'a -> 'a stat

val modstats0: 'a stat -> ('a -> 'a) -> unit
val modstats1: 'a stat -> ('a -> 'b -> 'a) -> 'b -> unit
val modstats2: 'a stat -> ('a -> 'b -> 'c -> 'a) -> 'b -> 'c -> unit

val register_stats_int: name:string -> init:int -> int stat
val incr: int stat -> unit
val decr: int stat -> unit

val print_stats: unit -> unit
