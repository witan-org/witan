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

(** Command line options *)


(** {2 Type definitions} *)

type language = Witan_solver.Input.language
(** Type of format input (taken from dolmen). *)

type input_options = {
  dir      : string;
  file     : string;
  language : language option;
}
(** The various input options. *)

type t = {

  (* Input options *)
  input : input_options;

  (* Time/Memory options *)
  time_limit  : float;
  size_limit  : float;
}
(** The aggregate type for all command line options *)


(** {2 Parsing command line} *)

val all : t Cmdliner.Term.t
(** The cdmliner term for parsing all command line options. *)

val info : Cmdliner.Term.info
(** The cmdliner info for parsing command line (includes bin name, version, etc..) *)

