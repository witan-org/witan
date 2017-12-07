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

type descr =
  | App of Term.Id.t * Node.t list
  | Other of Term.t
[@@ deriving show, ord, eq]

let hash_descr = function
  | App   (id,args) -> 2*(CCHash.pair Term.Id.hash (CCHash.list Node.hash) (id,args))
  | Other t         -> 2*(Term.hash t) + 1

let synsem = Sem.create_key "syntax"

module SynSem = RegisterSem(struct
    let key = synsem
    module T = struct
      type t = descr
      let compare = compare_descr
      let pp = pp_descr
      let hash = hash_descr
      let equal = equal_descr
    end
    include Stdlib.MkDatatype(T)
    include T
  end)

let index x ty = SynSem.node (SynSem.index x ty)

let const id = index (App(id,[])) (Id.ty id)

exception Type_mismatch of Node.t * Ty.t

let extract_fun_ty (ty:Term.t) =
  match ty with
  | { term = Binder (Arrow, v, ty); _ } -> v, ty
  | { term = Binder (Pi, _, _); _ } ->
    invalid_arg "dependent type not supported"
  | _ -> raise (Term.Function_expected ty)

let app id args =
  let apply_types ty arg =
    let v, res_ty = extract_fun_ty ty in
    let expected_arg_ty = Id.ty v in
    let actual_arg_ty = Node.ty arg in
    if Term.equal expected_arg_ty actual_arg_ty then
      res_ty
    else
      raise (Type_mismatch (arg, expected_arg_ty))
  in
  let rec get_res_type ty = function
    | [] -> ty
    | arg::l ->
      get_res_type (apply_types ty arg) l
  in
  let res_ty = get_res_type (Id.ty id) args in
  index (App(id,args)) res_ty

let rec of_term t =
  let f, args = Term.uncurry_app t in
  match f.Term.term with
  | Id id -> app id (List.map of_term args)
  | _ -> index (Other t) t.Term.ty

module DaemonRegisterTerm = struct
  let key = Demon.Fast.create "Synsem.DaemonRegisterTerm"

  module Data = Stdlib.DUnit

  let immediate = false
  let throttle = 100
  let wakeup d = function
    | Events.Fired.EventRegSem(nodesem,()) ->
      begin
        let nodesem = SynSem.coerce_nodesem nodesem in
        let v = SynSem.sem nodesem in
        match v with
        | Other _ -> ()
        | App(_,args) ->
          List.iter (Egraph.Delayed.register d) args
      end
    | _ -> raise UnwaitedEvent

end

module RDaemonRegisterTerm = Demon.Fast.Register(DaemonRegisterTerm)

let init env =
  RDaemonRegisterTerm.init env;
  Demon.Fast.attach env
    DaemonRegisterTerm.key [Demon.Create.EventRegSem(synsem,())]


let () = Exn_printer.register (fun fmt exn ->
    match exn with
    | Type_mismatch (t,ty) ->
      Format.fprintf fmt "Type mismatch %a is not of type %a."
        Node.pp t Term.print ty
    | exn -> raise exn
  )
