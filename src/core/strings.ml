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

(* useful function on string *)

let rev_split s c =
  let rec aux acc i =
    try
      let j = String.index_from s i c in
      aux ((String.sub s i (j-i))::acc) (j + 1)
    with Not_found -> (String.sub s i (String.length s - i))::acc
      | Invalid_argument _ -> ""::acc in
  aux [] 0

let ends_with s suf =
  let rec aux s suf suflen offset i =
    i >= suflen || (s.[i + offset] = suf.[i]
                   && aux s suf suflen offset (i+1)) in
  let slen = String.length s in
  let suflen = String.length suf in
  slen >= suflen && aux s suf suflen (slen - suflen) 0

let pad_right c s i =
  let sl = String.length s in
  if sl < i then
    let p = Bytes.create i in
    Bytes.blit_string s 0 p 0 sl;
    Bytes.fill p sl (i-sl) c;
    Bytes.unsafe_to_string p
  else if sl > i
  then String.sub s 0 i
  else s

module Make (X : sig end) = struct
  open Stdlib

  include DInt

  let hs = DStr.H.create 10
  let hi = DInt.H.create 10
  let c = ref (-1)

  let make s =
    try DStr.H.find hs s
    with Not_found ->
      let i = incr c; !c in (* >= 0 *)
      DStr.H.add hs s i;
      DInt.H.add hi i s;
      i

  let fresh s =
    let i = incr c; !c in (* >= 0 *)
    DInt.H.add hi i s;
    i

  let view i = DInt.H.find hi i

  let pp fmt i =
    try
      Format.pp_print_string fmt (view i)
    with Not_found -> Format.fprintf fmt "<unknown %i>" i

  let tag x = x

end

module Hashcons = Make (struct end)

open Stdlib

module type Fresh = sig
  type t = private int
  include Stdlib.Datatype with type t := t
  val create: string -> t
  val iter: (t -> unit) -> unit
  val hint_size: unit -> int
  val rename: t -> string -> unit
end


let find_new_name used_names s =
  let rec aux used_names  s n =
    let sid = s^(string_of_int n) in
    if DStr.H.mem used_names sid then
      aux used_names s (n+1)
    else sid,(n+1) in
  try
    let n = DStr.H.find used_names s in
    let s',n = aux used_names s n in
    DStr.H.replace used_names s n;
    DStr.H.add     used_names s' 2;
    s'
  with Not_found ->
    DStr.H.add     used_names s 2;
    s


module Fresh (X : sig end) = struct
  include DInt
  let names = Simple_vector.create 100
  let used_names : (* next id to use *) int DStr.H.t = DStr.H.create 100

  let pp fmt (x:t) =
    Format.pp_print_char fmt '@';
    Format.pp_print_string fmt (Simple_vector.get names (x:>int))

  let c = ref (-1)

  let create s =
    incr c;
    let i = !c in
    let s = find_new_name used_names s in
    Simple_vector.inc_size (i+1) names;
    Simple_vector.set names i s;
    i


  let iter f =
    for i = 0 to !c do
      f i
    done

  let hint_size () = !c + 1

  let rename i s =
    let s = find_new_name used_names s in
    Simple_vector.set names i s
end
