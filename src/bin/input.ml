
(* The Dolmen library is used to parse input languages *)
(* ************************************************************************ *)

exception File_not_found of string
(** Raised when file is not found. *)

(** See documentation at
    {{:http://gbury.github.io/dolmen/dev/Logic.Make.html} Logic.Make} *)
module P = Dolmen.Logic.Make
    (Dolmen.ParseLocation)
    (Dolmen.Id)
    (Dolmen.Term)
    (Dolmen.Statement)

(* Some re-export of definitions *)
type language = P.language =
  | Dimacs
  | ICNF
  | Smtlib
  | Tptp
  | Zf

let enum = P.enum

(** Convenience function to expand includes *)
let read_aux ~language ~dir input =
  let acc = ref [input] in
  let rec aux () =
    match !acc with
    | [] -> None
    | g :: r ->
      begin match g () with
        | None -> acc := r; aux ()
        | Some { Dolmen.Statement.descr = Dolmen.Statement.Include f; _ } ->
          let file = match P.find ~language ~dir f with
            | None -> raise (File_not_found f)
            | Some f -> f
          in
          let _, g' = P.parse_input ~language (`File file) in
          acc := g' :: !acc;
          aux ()
        | (Some _) as res -> res
      end
  in
  aux

let read ?language ~dir f =
  (** Formats Dimacs and Tptp are descriptive and lack the emission
      of formal solve/prove instructions, so we need to add them. *)
  let s = Dolmen.Statement.import f in
  (* Auto-detect input format *)
  let language =
    match language with
    | Some l -> l
    | None -> let res, _, _ = P.of_filename f in res
  in
  let g =
    match language with
    | P.Zf
    | P.ICNF
    | P.Smtlib -> Gen.singleton s
    | P.Dimacs
    | P.Tptp -> Gen.of_list [s; Dolmen.Statement.prove ()]
  in
  read_aux ~language ~dir g

