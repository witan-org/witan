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

(* Option types *)
(* ************************************************************************ *)

type language = Witan_solver.Input.language

type input_options = {
  dir      : string;
  file     : string;
  language : language option;
}

type t = {

  input : input_options;

  (* typing *)
  type_only   : bool;

  (* Time/Memory options *)
  time_limit  : float;
  size_limit  : float;
}

(* Creating option records *)
(* ************************************************************************ *)

let mk_input_options f language =
  let dir = Filename.dirname f in
  let file = Filename.basename f in
  { dir; file; language; }

let mk input time_limit size_limit type_only =
  { input; time_limit; size_limit; type_only }

(* Argument converters *)
(* ************************************************************************ *)

let input = Cmdliner.Arg.enum Witan_solver.Input.enum

(* Argument converter for integer with multiplier suffix *)
(* ************************************************************************ *)

let nb_sec_minute = 60
let nb_sec_hour = 60 * nb_sec_minute
let nb_sec_day = 24 * nb_sec_hour

let print_aux suffix fmt n =
  if n = 0 then ()
  else Format.fprintf fmt "%d%s" n suffix

let print_time fmt f =
  let n = int_of_float f in
  let aux n div = n / div, n mod div in
  let n_day, n = aux n nb_sec_day in
  let n_hour, n = aux n nb_sec_hour in
  let n_min, n = aux n nb_sec_minute in
  Format.fprintf fmt "%a%a%a%a"
    (print_aux "d") n_day
    (print_aux "h") n_hour
    (print_aux "m") n_min
    (print_aux "s") n

let parse_time arg =
  let l = String.length arg in
  let multiplier m =
    let arg1 = String.sub arg 0 (l-1) in
    `Ok (m *. (float_of_string arg1))
  in
  assert (l > 0);
  try
    match arg.[l-1] with
    | 's' -> multiplier 1.
    | 'm' -> multiplier 60.
    | 'h' -> multiplier 3600.
    | 'd' -> multiplier 86400.
    | '0'..'9' -> `Ok (float_of_string arg)
    | _ -> `Error "bad numeric argument"
  with Failure _ -> `Error "bad numeric argument"

let print_size fmt f =
  let n = int_of_float f in
  let aux n div = n / div, n mod div in
  let n_tera, n = aux n 1_000_000_000_000 in
  let n_giga, n = aux n 1_000_000_000 in
  let n_mega, n = aux n 1_000_000 in
  let n_kilo, n = aux n 1_000 in
  Format.fprintf fmt "%a%a%a%a%a"
    (print_aux "To") n_tera
    (print_aux "Go") n_giga
    (print_aux "Mo") n_mega
    (print_aux "ko") n_kilo
    (print_aux "") n

let parse_size arg =
  let l = String.length arg in
  let multiplier m =
    let arg1 = String.sub arg 0 (l-1) in
    `Ok (m *. (float_of_string arg1))
  in
  assert (l > 0);
  try
    match arg.[l-1] with
    | 'k' -> multiplier 1e3
    | 'M' -> multiplier 1e6
    | 'G' -> multiplier 1e9
    | 'T' -> multiplier 1e12
    | '0'..'9' -> `Ok (float_of_string arg)
    | _ -> `Error "bad numeric argument"
  with Failure _ -> `Error "bad numeric argument"

let c_time = parse_time, print_time
let c_size = parse_size, print_size

(* Command terms *)
(* ************************************************************************ *)

let common_options = "COMMON OPTIONS"

let man = [
  `S common_options;
  `P "Common options for the prover";
]

let info = Cmdliner.Term.(info ~man ~sdocs:common_options ~version:"0.1" "witan")

let input_options =
  let docs = common_options in
  let fd =
    let doc = "Input problem file" in
    Cmdliner.Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)
  in
  let language =
    let doc = Format.asprintf
        "Set the format for the input file to $(docv) (%s)."
        (Cmdliner.Arg.doc_alts_enum ~quoted:false Witan_solver.Input.enum) in
    Cmdliner.Arg.(value & opt (some input) None & info ["i"; "input"] ~docs ~docv:"INPUT" ~doc)
  in
  Cmdliner.Term.(const mk_input_options $ fd $ language)

let all =
  let docs = common_options in
  let time =
    let doc = {|Stop the program after a time lapse of $(docv).
                Accepts usual suffixes for durations : s,m,h,d.
                Without suffix, default to a time in seconds.|} in
    Cmdliner.Arg.(value & opt c_time 300. & info ["t"; "time"] ~docs ~docv:"TIME" ~doc)
  in
  let size =
    let doc = {|Stop the program if it tries and use more the $(docv) memory space.
                Accepts usual suffixes for sizes : k,M,G,T.
                Without suffix, default to a size in octet.|} in
    Cmdliner.Arg.(value & opt c_size 1_000_000_000. & info ["s"; "size"] ~docs ~docv:"SIZE" ~doc)
  in
  let type_only =
    let doc = {|Stop the program after parsing and typing.|} in
    Cmdliner.Arg.(value & flag & info ["type-only"] ~doc)
  in
  Cmdliner.Term.(const mk $ input_options $ time $ size $ type_only)
