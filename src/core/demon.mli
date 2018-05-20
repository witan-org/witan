(*************************************************************************)
(*  This file is part of Witan.                                          *)
(*                                                                       *)
(*  Copyright (C) 2017                                                   *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies           *)
(*           alternatives)                                               *)
(*    INRIA (Institut National de Recherche en Informatique et en        *)
(*           Automatique)                                                *)
(*    CNRS  (Centre national de la recherche scientifique)               *)
(*                                                                       *)
(*  you can redistribute it and/or modify it under the terms of the GNU  *)
(*  Lesser General Public License as published by the Free Software      *)
(*  Foundation, version 2.1.                                             *)
(*                                                                       *)
(*  It is distributed in the hope that it will be useful,                *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(*  GNU Lesser General Public License for more details.                  *)
(*                                                                       *)
(*  See the GNU Lesser General Public License version 2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).           *)
(*************************************************************************)

open Witan_popop_lib
open Nodes

module Create : sig
  type 'b event =
    (** the domain dom of the class is watched *)
    | EventDom      : Node.t * 'a DomKind.t  * 'b -> 'b event
    (** the value of the class is watched *)
    | EventValue    : Node.t * 'a ValueKind.t  * 'b -> 'b event
    (** the values of the class is watched *)
    | EventAnyValue    : Node.t  * 'b -> 'b event
    (** we want to register this class *)
    | EventRegCl    : Node.t           * 'b -> 'b event
    (** Warn when the class is not the representant of its eq-class anymore *)
    | EventChange   : Node.t           * 'b -> 'b event
    (** a new theory term 'a appear *)
    | EventRegSem   :        'a ThTermKind.t  * 'b -> 'b event
    (** a new value 'a appear *)
    | EventRegValue :      'a ValueKind.t  * 'b -> 'b event


  val pp: 'b event Format.printer

  type 'b t = 'b event list
end


type 'k alive =
| AliveReattached
| AliveStopped
| AliveRedirected of 'k

module Key: sig

  type ('k,'d,'i) t
  val create: string -> ('k,'d,'i) t

  module type S = sig
    module Key: Stdlib.Datatype

    module Data: Stdlib.Printable

    type info val default: info

    val key: (Key.t, Data.t, info) t

    val immediate: bool
    val wakeup:
      Egraph.t -> Key.t -> Data.t Events.Fired.t ->
      info -> Key.t alive
      (** the Events.t in wakeup is a subset of the one given in watch *)
  end

  module Register (D:S): sig
    val init: Egraph.t -> unit
    (** to run for each new delayed *)
  end

  type ('k,'i) state =
  | SUnborn
  | SAlive of 'i
  | SDead
  | SRedirected of 'k

  val attach: Egraph.t -> ('k,'d,'i) t -> 'k -> 'd Create.t -> unit
  (** raise AlreadyDead if this key is already dead *)

  val is_attached: Egraph.t -> ('k,'d,'i) t -> 'k -> ('k,'i) state

  val set_info: Egraph.t -> ('k, 'd, 'i) t -> 'k -> 'i -> unit

  exception CantBeKilled
  val kill : Egraph.t -> ('a, 'b,'c) t -> 'a -> unit


end


module Fast: sig

  type 'd t
  val create: string -> 'd t

  module type S = sig

    module Data: sig
      type t
      val pp: t Format.printer
    end

    val key: Data.t t

    (** never killed *)
    val immediate: bool
    val throttle: int (** todo int ref? *)
    (** number of time run in a row *)
    val wakeup: Egraph.t -> Data.t Events.Fired.event -> unit

  end

  module Register (D:S): sig
    val init: Egraph.t -> unit
    (** to run for each new delayed *)
  end

  val attach: Egraph.t -> 'd t -> 'd Create.t -> unit

  (** helper *)
  val register_init_daemon:
    name:string ->
    ?immediate:bool ->
    ?throttle:int ->
    (module RegisteredThTerm with type t = 'a) ->
    (Egraph.t -> 'a -> unit) ->
    Egraph.t ->
    unit
    (** *)

  (** helper *)
  val register_init_daemon_value:
    name:string ->
    ?immediate:bool ->
    ?throttle:int ->
    (module RegisteredValue with type t = 'a) ->
    (Egraph.t -> 'a -> unit) ->
    Egraph.t ->
    unit
    (** *)
end
