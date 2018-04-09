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

open OUnit
open Witan_stdlib
open Witan_core
open Tests_lib


let opt_seed = ref 0

let print_seed fmt = function
  | None -> Format.fprintf fmt "No"
  | Some [|i|] -> Format.fprintf fmt "%i" i
  | _ -> assert false

let make_tests acc seed =
  let test = ((Witan_popop_lib.Pp.sprintf "seed %a" print_seed seed) >:::
                 [ Tests_bool.tests; Tests_uf.tests; Tests_LRA.tests ])
  in
  let test = test_decorate
    (fun f -> (fun () -> Shuffle.set_shuffle seed; f ())) test in
  test::acc

let tests () =
  let l = Witan_popop_lib.Util.foldi (fun acc i -> make_tests acc (Some [|i|])) []
    (!opt_seed + 1) (!opt_seed + 9)in
  make_tests l None

let tests () =
  if Printexc.backtrace_status ()
  then
    (test_decorate
       (fun f ->
          fun () ->
            try f ()
            with exn ->
              Format.fprintf (Witan_popop_lib.Debug.get_debug_formatter ()) "%s"
                (Printexc.get_backtrace ());
              raise exn
       )) (TestList (tests ()))
  else
    (TestList (tests ()))

(** From oUnit.ml v 1.2.2 *)
(** just need to make the tests lazily computed *)

(* Returns true if the result list contains successes only *)
let rec was_successful =
  function
    | [] -> true
    | RSuccess _::t
    | RSkip _::t ->
        was_successful t

    | RFailure _::_
    | RError _::_
    | RTodo _::_ ->
        false


(* Call this one from you test suites *)
let run_test_tt_main ?(arg_specs=[]) suite =
  let only_test = ref [] in
  let () =
    Arg.parse
      (Arg.align
         [
           "-only-test",
           Arg.String (fun str -> only_test := str :: !only_test),
           "path Run only the selected test";

           "-list-test",
           Arg.Unit
             (fun () ->
                List.iter
                  (fun pth ->
                     print_endline (string_of_path pth))
                  (test_case_paths (suite ()));
                exit 0),
           " List tests";
         ] @ arg_specs
      )
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      ("usage: " ^ Sys.argv.(0) ^ " [-verbose] [-only-test path]*")
  in
  let () = Witan_popop_lib.Debug.Args.set_flags_selected () in
  let verbose = Witan_popop_lib.Debug.test_flag debug in
  let nsuite =
    if !only_test = [] then
      suite ()
    else
      begin
        match test_filter ~skip:true !only_test (suite ()) with
          | Some test ->
              test
          | None ->
              failwith ("Filtering test "^
                        (String.concat ", " !only_test)^
                        " lead to no test")
      end
  in
  let result = run_test_tt ~verbose nsuite in
    if not (was_successful result) then
      exit 1
    else
      result

(*** End *)

let () =
  if not (Egraph.check_initialization ()) then
    exit 1

let _ =
  try
    run_test_tt_main
      ~arg_specs:(["--seed",Arg.Set_int opt_seed,
                  " Base seed used for shuffling the arbitrary decision";
                       Witan_popop_lib.Debug.Args.desc_debug_all]@
                  Witan_popop_lib.Debug.Args.desc_debug)
      tests
  with e when not (Witan_popop_lib.Debug.test_flag Witan_popop_lib.Debug.stack_trace) ->
    Format.eprintf "%a" Witan_popop_lib.Exn_printer.exn_printer e;
    exit 1
