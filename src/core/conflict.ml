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

(** Decision, Conflict and Learning *)

(** {2 Decision} *)
module Cho = Explanation.Cho

type 'd decdone  =
| DecNo (** No decision to do *)
| DecTodo of 'd (** This decision should be done *)

module type Cho = sig
  (** Allows to keep any information for the potential decision *)
  module OnWhat  : sig
    type t
    val pp: t Pp.pp
  end

  (** Allows to transfer any information from {!choose_decision} and {!make_decision} *)
  module What: sig
    type t
    val pp: t Pp.pp
  end

  val choose_decision:
    Solver.Delayed.t -> OnWhat.t -> What.t decdone
  (** Answer the question: Is the decision still needed? *)

  val make_decision:
    Solver.Delayed.t -> Explanation.dec -> OnWhat.t -> What.t -> unit
  (** Propagate the decision using {!Solver.Delayed.t} *)

  val key: (OnWhat.t,What.t) Cho.t

end

(** {2 Conflict} *)

module Conflict = struct
  type t
end

module Exp = Explanation.Exp
module Con = Keys.Make_key(struct end)

type congen =
  | GCon: 'a Con.t * 'a -> congen

module type Exp = sig

  type t

  val pp: t Pp.pp

  val key: t Explanation.Exp.t

  val analyse  :
    Conflict.t -> Explanation.Age.t -> t -> 'a Con.t -> 'a -> congen
    (** One step of the analysis done on the trail. If the explanation
        as nothing to do with this conflict it should return it
        unchanged
    *)
end

let register_exp: (module Exp) -> unit = fun _ -> assert false

let contradiction = Con.create_key "contradiction"
(** The start of the conflict analysis. It is false. It should be
    replaced by {!Exp.analyse} by the explanation given to
    {!Delayed.contradiction}
 *)

(** {2 Learning} *)

module type Con = sig

  type t

  val pp: t Pp.pp

  val apply_learnt: t -> Typedef.Node.t
  (** Build the constraint that correspond to the conflict learnt *)
end

let register_con: (module Con) -> unit = fun _ -> assert false
