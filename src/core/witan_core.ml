(*************************************************************************)
(*  This file is part of Witan.                                          *)
(*                                                                       *)
(*  Copyright (C) 2017                                                   *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies           *)
(*           alternatives)                                               *)
(*    INRIA (Institut National de Recherche en Informatique et en        *)
(*           Automatique)                                                *)
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

include Std

exception UnwaitedEvent = Typedef.UnwaitedEvent

module Ty = struct
  include Typedef.Ty
end

module Cl = struct
  include Typedef.Cl
end

module Dom = struct
  include Typedef.Dom
  type 'a t = 'a k
  let print = Solver.print_dom

  module type Dom = Solver.Dom

  module Register = Solver.RegisterDom
end

module Sem = struct
  include Typedef.Sem
  type 'a t = 'a k
  let print = Typedef.print_sem

  module type Sem = Typedef.Sem
  module type Registered = Typedef.RegisteredSem

  module Register = Typedef.RegisterSem
end

module Dem = struct
  include Typedef.Dem
  type ('a,'b) t = ('a,'b) k

  module type Dem = Solver.Dem

  module Register = Solver.RegisterDem
end

module Solver = Solver
module Demon = Demon
module Explanation = Explanation
module Variable = Variable
