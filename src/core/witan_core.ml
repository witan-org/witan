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

(** Witan core: define basic types and the solver *)

module Id = Id
module Term = Term
module Ty = Ty

module Keys = Keys

module Node = struct
  include Nodes.Node
end

module ThTermKind = struct
  include Nodes.ThTermKind
  let print = Nodes.print_thterm

  module type ThTerm = Nodes.ThTerm
  module type Registered = Nodes.RegisteredThTerm

  module Register = Nodes.RegisterThTerm
end

module ThTerm = Nodes.ThTerm

module SynTerm = SynTerm

module ValueKind = struct
  include Nodes.ValueKind
  let print = Nodes.print_value

  module type Value = Nodes.Value
  module type Registered = Nodes.RegisteredValue

  module Register = Nodes.RegisterValue

  let get = Nodes.get_value
  let get_registered = Nodes.get_registered_value
end

module Value = Nodes.Value

module Interp = Interp

module DomKind = struct
  include DomKind
  let print = Egraph.print_dom

  module type Dom = Egraph.Dom

  let register = Egraph.register_dom
end

module Dem = struct
  include Events.Dem

  module type Dem = Egraph.Wait.Dem

  let register = Egraph.Wait.register_dem
end

module Env = Env

module Exp = Trail.Exp

module Egraph = Egraph
module Trail = Trail

module Events = Events
module Demon  = Demon

module Conflict = Conflict

exception UnwaitedEvent = Nodes.UnwaitedEvent
(** Can be raised by daemon when receiving an event that they don't
    waited for. It is the sign of a bug in the core solver *)
