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

(** Problem input for Witan *)

(** {2 Parsing some input} *)

type language =
  | Dimacs  (** The dimacs language *)
  | ICNF    (** iCNF is ane xtension of dimacs *)
  | Smtlib  (** smtlib language *)
  | Tptp    (** TPTP problems language *)
  | Zf      (** Zipperposition format *)
(** The type of input language supported. *)

val enum : (string * language) list
(** Enumeration of pairs of a language and its name, mainly for use by cmdliner. *)

val read : ?language:language -> dir:string -> string -> Dolmen.Statement.t Gen.t
(** Read a file in a directory. Automatically expands all include statements.
    @language: if set, overrides input language auto-detection performed by dolmen. *)
