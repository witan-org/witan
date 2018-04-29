(* This module contains the basic constructions of Patricia trees to
  represent maps and sets *)

open Patricia_sig

(* type ('keys,'values,'common,'branching,'infos) poly *)

module Make(K:Key) : sig

  module MapH(I:MapArgH with type t:=K.t)
    : MapH with type keys    = K.t
            and type common  := K.common
            and type branching := K.branching
            and type values  = I.values
            and type infos   = I.infos

  module MapNH(I:MapArgNH with type t:=K.t)
    : Map with type keys    = K.t
           and type common  := K.common
           and type branching := K.branching
           and type values  = I.values
           and type infos   = I.infos

  module SetH(I:SetArgH with type t:=K.t)
    : SetH with type e       = K.t
            and type common  := K.common
            and type branching := K.branching
            and type infos   = I.infos

  module SetNH(I:SetArgNH with type t:=K.t)
    : Set with type e       = K.t
           and type common  := K.common
           and type branching := K.branching
           and type infos   = I.infos
end
