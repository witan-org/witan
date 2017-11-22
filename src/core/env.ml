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

module Env = Keys.Make_key(struct end)

include Env


module VEnv = Env.MkVector(struct type ('a,'b) t = 'a Pp.pp end)
let defined_env = VEnv.create 8
let print_env k =
  assert (if VEnv.is_uninitialized defined_env k
    then raise Typedef.UnregisteredKey else true);
  VEnv.get defined_env k

let register_env pp env =
  VEnv.inc_size env defined_env;
  assert (if not (VEnv.is_uninitialized defined_env env)
          then raise Typedef.AlreadyRegisteredKey else true);
  VEnv.set defined_env env pp

let check_is_registered k =
  assert (if VEnv.is_uninitialized defined_env k
          then raise Typedef.UnregisteredKey else true);
