(****************************************************)
(* This module is to be opened before anything else *)
(* as it redefines OCaml's standard stuff           *)
(****************************************************)

module Pervasives = Pervasives

include Pervasives

let lex_compare compare1 compare2 (a1,a2) (b1,b2)
  = let c = compare1 a1 b1 in
    if c = 0 then compare2 a2 b2 else c

let min ~compare a1 a2 =
  if compare a1 a2 <= 0 then a1 else a2

let max ~compare a1 a2 =
  if compare a1 a2 >= 0 then a1 else a2

let show_of_pp pp a = 
  let buf = Buffer.create 255 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "%a%!" pp a;
  Buffer.contents buf

module List = struct

  include List
            
  type 'a t = 'a list [@@deriving eq]

  let pp ?(sep="; ") ?(wrap="[","]") f fmt l =
    let rec aux fmt = function
    | []   -> Format.fprintf fmt ""
    | [a]  -> Format.fprintf fmt "%a" f a
    | a::l -> Format.fprintf fmt "%a%s%a" f a sep aux l
    in
    let b,e = wrap in
    Format.fprintf fmt "%s%a%s" b aux l e
       
  let rec mem ~equal x = function
    | [] -> false
    | y::_ when equal x y -> true
    | _::l -> mem ~equal x l

  let fold f seed l = List.fold_left (fun sofar elt -> f elt sofar) l seed

end
