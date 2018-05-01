include Std_sig

let nnil = function [] -> false | _::_ -> true
  
module Poly = struct

  type (_,_,_) t =
    | Eq : ('a,'a,[< `Eq | `IsEq | `Ord]) t
    | Neq: (_,_,[`IsEq]) t
    | Gt : (_,_,[`Ord]) t
    | Lt : (_,_,[`Ord]) t

  type ('a,'b) eq   = ('a,'b,[`Eq]) t
  type ('a,'b) iseq = ('a,'b,[`IsEq]) t
  type ('a,'b) ord  = ('a,'b,[`Ord]) t

  exception NotEq
    
  let iseq (type a b) : (a,b,[< `Eq | `IsEq | `Ord]) t -> (a,b) iseq = function
    | Eq -> Eq
    | _ -> Neq

  let eq (type a b) : (a,b,[< `Eq | `IsEq | `Ord]) t -> (a,b) eq = function
    | Eq -> Eq
    | _ -> raise NotEq
  
end

module Goption = struct
  type (_,_) t =
    | Some: 'a -> ('a,[`Some]) t
    | None:       ('a,[`None]) t
end

(* Extending Q module from Zarith *)
module Q = struct
  module Arg = struct
    include Q (* Module from Zarith *)
    let hash = Hash.poly
    let pp fmt q =
      Format.(
        match Q.classify q with
        | Q.ZERO  -> char fmt '0'
        | Q.INF   -> string fmt "+∞"
        | Q.MINF  -> string fmt "-∞"
        | Q.UNDEF -> string fmt "!undef!"
        | Q.NZERO -> Q.pp_print fmt q
      )
  end
  include Arg
  include Witan_popop_lib.Stdlib.MkDatatype(Arg)
  let two = Q.of_int 2
  let ge = Q.geq
  let le = Q.leq
    
  let of_string_decimal =
    let decimal = Str.regexp "\\(+\\|-\\)?\\([0-9]+\\)\\([.]\\([0-9]*\\)\\)?" in
    fun s ->
      if not (Str.string_match decimal s 0) then None
      else
        let sgn = match Str.matched_group 1 s with
          | "-" -> Q.minus_one
          | "+" -> Q.one
          | exception Not_found -> Q.one
          | _ -> assert false in
        let int_part = Q.of_string (Str.matched_group 2 s) in
        let dec_part = match Str.matched_group 4 s with
          | exception Not_found -> Q.zero
          | "" -> Q.zero
          | dec ->
            let l = String.length dec in
            let dec = Q.of_string dec in
            let ten = Q.of_int 10 in
            Witan_popop_lib.Util.foldi (fun acc _ -> Q.(acc * ten)) dec 1 l
        in
        Some Q.(sgn * (int_part + dec_part))

end
