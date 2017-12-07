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

include Std

module Id = Id
module Term = Term
module Ty = Ty

module Keys = Keys

module Node = struct
  include Typedef.Node
end

module Sem = struct
  include Typedef.Sem
  let print = Typedef.print_sem

  module type Sem = Typedef.Sem
  module type Registered = Typedef.RegisteredSem

  module Register = Typedef.RegisterSem
end

module NodeSem = Typedef.NodeSem

module Synsem = Synsem

module Value = struct
  include Typedef.Value
  let print = Typedef.print_value

  module type Value = Typedef.Value

  module Register = Typedef.RegisterValue
end

module NodeValue = Typedef.NodeValue


module Dom = struct
  include Dom
  let print = Egraph.print_dom

  module type Dom = Egraph.Dom

  let register = Egraph.register_dom
end

module Dem = struct
  include Typedef.Dem

  module type Dem = Egraph.Wait.Dem

  let register = Egraph.Wait.register_dem
end

module Env = Env

module Exp = Trail.Exp

module Egraph = Egraph
module Demon = Demon
module Trail = Trail
module Conflict = Conflict

module Variable = Variable

module Events = Events

exception UnwaitedEvent = Typedef.UnwaitedEvent
(** Can be raised by daemon when receiving an event that they don't
    waited for. It is the sign of a bug in the core solver *)
