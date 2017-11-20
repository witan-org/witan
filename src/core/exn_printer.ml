(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2017   --   INRIA - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

type exn_printer = exn Pp.pp

let exn_printers =
  (Stack.create () : (exn Pp.pp) Stack.t)

let register exn_printer = Stack.push exn_printer exn_printers

exception Exit_loop

let exn_printer fmt exn =
  let test f =
    try
      Format.fprintf fmt "@[%a@]" f exn;
      raise Exit_loop
    with
      | Exit_loop -> raise Exit_loop
      | _ -> ()
  in
  try
    Stack.iter test exn_printers;
    raise exn
  with Exit_loop -> ()


let () =
  Printexc.register_printer (fun exn -> Some (Pp.string_of exn_printer exn))


(** usual version *)
let exn_printer fmt exn =
  try
    exn_printer fmt exn
  with _ ->
    Format.fprintf fmt "anomaly: %s" (Printexc.to_string exn)
