(**********************************************************)
(* This file contains the implementation of HConsed types *)
(**********************************************************)

include module type of HCons_sig

module MakePoly(M: PolyArg) : sig

  include PolyS with type ('t,'a) initial := ('t,'a) M.t

  module InitData
      (Par: sig type t [@@deriving eq] val hash: t Hash.t end)
      (Data: sig
         type t
         val build : (Par.t,t*[`HCons]) generic -> t
       end)
    : SHCons with type t        = (Par.t,Data.t*[`HCons]) generic 
              and type revealed = (Par.t,Data.t*[`HCons]) g_revealed

  module Init(Par: sig type t [@@deriving eq] val hash:t Hash.t end)
    : SHCons with type t        = (Par.t,unit*[`HCons]) generic 
              and type revealed = (Par.t,unit*[`HCons]) g_revealed

end

module Make(M: Arg) : sig

  include S with type 't initial := 't M.t

  module InitData
      (Data: sig
         type t
         val build : (t*[`HCons]) generic -> t
       end)
    : SHCons with type t        = (Data.t*[`HCons]) generic 
              and type revealed = (Data.t*[`HCons]) g_revealed

  module Init()
    : SHCons with type t        = (unit*[`HCons]) generic 
              and type revealed = (unit*[`HCons]) g_revealed

end
