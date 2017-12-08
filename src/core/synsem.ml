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

open Typedef

let synsem = Sem.create_key "syntax"

module Sem = RegisterSem(struct
    let key = synsem
    include Stdlib.MkDatatype(Term)
    include Term
  end)

let node_of_term x = Sem.node (Sem.index x (x.Term.ty))

let converters = Env.create_key "Synsem.converters"

let register_converter env r =
  let l = Egraph.Delayed.get_env env converters in
  Egraph.Delayed.set_env env converters (r::l)

let () = Env.register_env (fun _ _ -> ()) converters

module DaemonConvertTerm = struct
  let key = Demon.Fast.create "Synsem.DaemonConvertTerm"

  module Data = Stdlib.DUnit

  let immediate = false
  let throttle = 100
  let wakeup d = function
    | Events.Fired.EventRegSem(nodesem,()) ->
      begin try begin
        let nodesem = Sem.coerce_nodesem nodesem in
        let v = Sem.sem nodesem in
        let f, l = Term.uncurry v in
        let iter conv =
          match conv d f l with
          | None -> ()
          | Some node ->
            Egraph.Delayed.register d node;
            Egraph.Delayed.merge d Trail.pexpfact (Sem.node nodesem) node
        in
        List.iter iter (Egraph.Delayed.get_env d converters)
      end with Exit -> () end
    | _ -> raise UnwaitedEvent

end

module RDaemonConvertTerm = Demon.Fast.Register(DaemonConvertTerm)

let init env =
  Egraph.Delayed.set_env env converters [];
  RDaemonConvertTerm.init env;
  Demon.Fast.attach env
    DaemonConvertTerm.key [Demon.Create.EventRegSem(Sem.key,())];

include Sem
