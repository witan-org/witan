exception Impossible (* Absurd *)
exception TODO of string

type (_,_) eq = Eq : ('a,'a) eq

module type TaggedType =
sig
  type t
  val tag : t -> int
  val pp:  t Format.printer
end

module type OrderedHashedType =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp: t Format.printer
end

(* module type Datatype = sig
 *   include OrderedHashedType
 * 
 *   module M : Map_intf.PMap with type key = t
 *   module S : Map_intf.Set with type 'a M.t = 'a M.t
 *                            and type M.key = M.key
 *   module H : Exthtbl.Hashtbl.S with type key = t
 * end *)

