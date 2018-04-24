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

open Nodes

let synsem = ThTermKind.create_key (module struct type t = Term.t let name = "syntax" end)

module ThTerm = RegisterThTerm(struct
    let key = synsem
    include Term
  end)

let node_of_term x = ThTerm.node (ThTerm.index x (x.Term.ty))


type env = {
  converters: (Egraph.t -> Term.t -> Term.t list -> Node.t option) list;
  decvars: (Node.t -> Trail.chogen option) list;
}

let converters = Env.create_key (module struct
    type t = env
    let name = "Synsem.converters"
  end)

let register_converter env r =
  let e = Egraph.get_env env converters in
  Egraph.set_env env converters {e with converters = r::e.converters}

let register_decvars env r =
  let e = Egraph.get_env env converters in
  Egraph.set_env env converters {e with decvars = r::e.decvars}

let () = Env.register (fun _ _ -> ()) converters

let letin v e t =
  match t.Term.term with
  | Term.Id id when Term.Id.equal id v -> e
  | Term.Id _ -> t
  | _ -> Term.letin v e t

let uncurry_app t =
  let lets t l = List.fold_left (fun t (v,e) -> letin v e t) t l in
  let rec aux binders args = function
    | { Term.term = App (f, arg); _ } ->
      aux binders ((lets arg binders) :: args) f
    | { Term.term = Let (v,e,t); _ } ->
      aux ((v,e)::binders) args t
    | { Term.term = Id id; _ } as t ->
      let rec find = function
        | [] -> t, args
        | (v,e)::binders when Id.equal v id ->
          aux binders args e
        | _::binders ->
          find binders
      in
      find binders
    | t -> t, args
  in
  aux [] [] t


module DaemonConvertTerm = struct
  let key = Demon.Fast.create "Synsem.DaemonConvertTerm"

  module Data = Stdlib.DUnit

  let immediate = false
  let throttle = 100
  let wakeup d = function
    | Events.Fired.EventRegSem(thterm,()) ->
      begin try begin
        let e = Egraph.get_env d converters in
        let thterm = ThTerm.coerce_thterm thterm in
        let v = ThTerm.sem thterm in
        let f, l = uncurry_app v in
        begin match f with
          | {Term.term = Id id} when not (Term.is_defined id) ->
            let n = ThTerm.node thterm in
            List.iter (fun f ->
                Opt.iter
                  (Egraph.register_decision d)
                  (f n)
              ) e.decvars
          | _ -> ()
        end;
        let iter conv =
          match conv d f l with
          | None -> ()
          | Some node ->
            Egraph.register d node;
            Egraph.merge d Trail.pexp_fact (ThTerm.node thterm) node
        in
        List.iter iter e.converters
      end with Exit -> () end
    | _ -> raise UnwaitedEvent

end

module RDaemonConvertTerm = Demon.Fast.Register(DaemonConvertTerm)

let init env =
  Egraph.set_env env converters {converters=[]; decvars = []};
  RDaemonConvertTerm.init env;
  Demon.Fast.attach env
    DaemonConvertTerm.key [Demon.Create.EventRegSem(ThTerm.key,())];

include ThTerm
