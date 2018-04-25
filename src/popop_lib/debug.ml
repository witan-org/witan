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

let noassert = let x = ref true in assert (x := false; true); !x

let formatter = ref Format.err_formatter

let () = Format.pp_set_margin !formatter 300

let () =
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle
       (fun _ -> print_endline "Stopped by user"; exit 1))

exception UnknownFlag of string

type flag = bool ref

let _true = ref true
let todo  = _true
let modifiable s = s != _true

let flag_table = Hashtbl.create 17

let fst3 (flag,_,_) = flag
let snd3 (_,info,_) = info
let thd3 (_,_,desc) = desc

let gen_register_flag (desc : Pp.formatted) s info =
  try
    fst3 (Hashtbl.find flag_table s)
  with Not_found ->
    let flag = ref false in
    Hashtbl.replace flag_table s (flag,info,desc);
    flag

let register_info_flag ~desc s = gen_register_flag desc s true
let register_flag      ~desc s = gen_register_flag desc s false

let list_flags () =
  Hashtbl.fold (fun s (v,_,desc) acc -> (s,v,!v,desc)::acc) flag_table []

let lookup_flag s =
  try fst3 (Hashtbl.find flag_table s) with Not_found -> raise (UnknownFlag s)

let is_info_flag s =
  try snd3 (Hashtbl.find flag_table s) with Not_found -> raise (UnknownFlag s)

let flag_desc s =
  try thd3 (Hashtbl.find flag_table s) with Not_found -> raise (UnknownFlag s)

let test_flag s = !s
let test_noflag s = not !s

let set_flag s = assert (modifiable s); s := true
let unset_flag s = assert (modifiable s); s := false
let toggle_flag s = assert (modifiable s); s := not !s

let () = Exn_printer.register (fun fmt e -> match e with
  | UnknownFlag s -> Format.fprintf fmt "unknown debug flag `%s'" s
  | _ -> raise e)

let stack_trace = register_flag "stack_trace"
  ~desc:"Avoid@ catching@ exceptions@ in@ order@ to@ get@ the@ stack@ trace."

let timestamp = register_info_flag "timestamp"
  ~desc:"Print@ a@ timestamp@ before@ debugging@ messages."


let time_start = Unix.gettimeofday ()


let set_debug_formatter fmt =
  (** enable the usual behavior of stderr: flush at every new line *)
  let out = Format.pp_get_formatter_out_functions fmt () in
  Format.pp_set_formatter_out_functions
    fmt {out with out_newline =
                    (fun () ->
                       out.out_newline ();
                       out.out_flush ();
                       if !timestamp then
                         let s =
                           Printf.sprintf "<%f>"
                             (Unix.gettimeofday () -. time_start) in
                         out.out_string s 0 (String.length s) ;
                    ) };
  Format.pp_open_vbox fmt 0;
  formatter := fmt

let get_debug_formatter () = !formatter

let () = set_debug_formatter Format.err_formatter

let real_dprintf ?nobox s =
  let box = match nobox with None -> true | Some () -> false in
  if box then begin
    Format.pp_print_cut !formatter ();
    Format.pp_open_box !formatter 0;
  end;
  Format.kfprintf
    (fun fmt -> if box then begin
         Format.pp_close_box fmt ();
       end
    )
    !formatter s

let dprintf0 ?nobox flag s =
  if !flag then real_dprintf ?nobox s

let dprintf1 ?nobox flag s a1 =
  if !flag then real_dprintf ?nobox s a1

let dprintf2 ?nobox flag s a1 a2 =
  if !flag then real_dprintf ?nobox s a1 a2

let dprintf3 ?nobox flag s a1 a2 a3 =
  if !flag then real_dprintf ?nobox s a1 a2 a3

let dprintf4 ?nobox flag s a1 a2 a3 a4 =
  if !flag then real_dprintf ?nobox s a1 a2 a3 a4

let dprintf5 ?nobox flag s a1 a2 a3 a4 a5 =
  if !flag then real_dprintf ?nobox s a1 a2 a3 a4 a5

let dprintf6 ?nobox flag s a1 a2 a3 a4 a5 a6 =
  if !flag then real_dprintf ?nobox s a1 a2 a3 a4 a5 a6

let dprintf7 ?nobox flag s a1 a2 a3 a4 a5 a6 a7 =
  if !flag then real_dprintf ?nobox s a1 a2 a3 a4 a5 a6 a7

let dprintf8 ?nobox flag s a1 a2 a3 a4 a5 a6 a7 a8 =
  if !flag then real_dprintf ?nobox s a1 a2 a3 a4 a5 a6 a7 a8

let dprintf9 ?nobox flag s a1 a2 a3 a4 a5 a6 a7 a8 a9 =
  if !flag then real_dprintf ?nobox s a1 a2 a3 a4 a5 a6 a7 a8 a9

let dprintf10 ?nobox flag s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
  if !flag then real_dprintf ?nobox s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10

let dprintf11 ?nobox flag s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
  if !flag then real_dprintf ?nobox s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11

let dprintf12 ?nobox flag s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
  if !flag then real_dprintf ?nobox s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12

let dprintf13 ?nobox flag s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 =
  if !flag then real_dprintf ?nobox s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13

let dprintf14 ?nobox flag s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 =
  if !flag then real_dprintf ?nobox s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14

let dprintfn ?nobox flag s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 =
  if !flag then real_dprintf ?nobox s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15
  else
    (* ifprintf take too many times for computing the format *)
    let rec aux = fun _ -> Obj.magic aux in
    Obj.magic aux

(*** Command-line arguments ****)

module Args = struct
  type spec = (Arg.key * Arg.spec * Arg.doc)

  let desc_debug_list, option_list =
    let opt_list_flags = ref false in
    let desc =
      "--list-debug-flags", Arg.Set opt_list_flags,
      " List known debug flags" in
    let list () =
      if !opt_list_flags then begin
        let list =
          Hashtbl.fold (fun s (_,info,desc) acc -> (s,info,desc)::acc)
            flag_table [] in
        let pp fmt (p,info,desc) =
          Format.fprintf fmt "@[%s%s  @[%a@]@]"
            p (if info then " *" else "")
            Pp.formatted desc
        in
        Format.printf "@[<v 2>@[Known debug flags \
            (`*' marks the flags selected by --debug-all):@]@,%a@]"
          (Pp.list Pp.newline pp)
          (List.sort Pervasives.compare list);
      end;
      !opt_list_flags in
    desc,list

  let opt_list_flags = ref []

  let add_flag s = opt_list_flags := s::!opt_list_flags
  let add_all_flags () =
      Hashtbl.iter (fun s (_,info,_) -> if info then add_flag s) flag_table
  let remove_flag s =
    opt_list_flags := List.filter (fun x -> x <> s) !opt_list_flags

  let desc_shortcut flag option desc =
    let set_flag () = add_flag flag in
    let desc = Pp.sprintf "%s (same as --debug %s)" desc flag in
    (option, Arg.Unit set_flag, desc)

  let desc_debug =
    ["--debug", Arg.String add_flag, "<flag> Set a debug flag";
     "--no-debug", Arg.String remove_flag, "<flag> Remove a debug flag"]

  let desc_debug_all =
    let desc_debug =
      Pp.sprintf
        " Set all debug flags that do not change Why3 behaviour" in
    ("--debug-all", Arg.Unit add_all_flags, desc_debug)

  let set_flags_selected () =
    List.iter (fun flag -> let flag = lookup_flag flag in set_flag flag)
      !opt_list_flags;
    if test_flag stack_trace then Printexc.record_backtrace true
end

(** Stats *)
let stats = register_info_flag "stats"
  ~desc:"Compute and pp statistics."

type 'a stat = 'a ref

let max_name_size = ref 0


type stats =
| Stat : ('a Pp.pp) * string * 'a ref -> stats

let registered_stats : stats list ref = ref []

let rec print_nb_char fmt = function
  | n when n <= 0 -> ()
  | n -> Format.pp_print_char fmt ' '; print_nb_char fmt (n-1)

let print_stat fmt (Stat(pp,name,r)) =
  Format.fprintf fmt "@[%s%a: %a@]"
    name print_nb_char (!max_name_size - String.length name)
    pp !r

let print_stats () =
  dprintf2 stats "@[%a@]@\n"
    (Pp.list Pp.newline print_stat)
    !registered_stats



let () = at_exit (fun () ->
    print_stats ();
    Format.pp_print_flush !formatter ())
(** SIGXCPU cpu time limit reached *)
let _ =
  (** TODO? have a possible callback for printing different message*)
  Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> exit 2))

let register_stats ~pp ~name ~init =
  let s = ref init in
  max_name_size := max !max_name_size (String.length name);
  registered_stats := Stat(pp,name,s)::!registered_stats;
  s

let modstats0 r f =
  if test_flag stats then r := f !r
let modstats1 r f x =
  if test_flag stats then r := f !r x
let modstats2 r f x y =
  if test_flag stats then r := f !r x y

let register_stats_int ~name ~init =
  register_stats ~pp:Format.pp_print_int ~name ~init

let incr r = if test_flag stats then incr r
let decr r = if test_flag stats then decr r

