(**********************************************************)
(* This file contains the implementation of HConsed types *)
(**********************************************************)

include HCons_sig

module EmptyData = struct 
  type t = unit
  let build _ = ()
end

module MakePoly(M: PolyArg) = struct

  let tableid = ref 0

  type (_,_) generic =
      G : {reveal: ('a,'data*'hcons) g_revealed;
           id    : int;
           data  : 'data Lazy.t }
      -> ('a,'data*'hcons) generic 

  and ('a,'dh) g_revealed = (('a,'dh) generic,'a) M.t

  let reveal (G{reveal}) = reveal
  let data   (G{data})   = Lazy.force data
  let noHCons reveal data = G{ reveal; id = 0; data }
    
  module InitData
      (Par: sig type t [@@deriving eq] val hash: t Hash.t end)
      (Data: sig
         type t
         val build : (Par.t,t*[`HCons]) generic -> t
       end)
  = struct

    let () = incr tableid
    let tableid = !tableid
    (* let () = Print.print ["HCons",1] (fun p ->
     *              p "Creating hash-consing table %s%i" M.name tableid) *)

    type t        = (Par.t,Data.t*[`HCons]) generic
    type revealed = (Par.t,Data.t*[`HCons]) g_revealed

    let id (G f) = f.id
    let hash  = id
    let equal = Equal.physical
    let compare a b = Ord.(int >|= id) a b

    module Arg = struct
      type t = revealed
      let equal = M.equal Equal.physical Par.equal
      let hash  = M.hash hash Par.hash
    end
    (* module Arg = struct *)
    (*   type nonrec t = t *)
    (*   let equal a b = M.equal (==) Par.equal a.reveal b.reveal *)
    (*   let hash a    = M.hash id Par.hash a.reveal *)
    (* end *)

    module H = Hashtbl.Make(Arg)
    (* module H = Weak.Make(Arg) *)
    let table   = H.create 5003
    let unique  = ref 0

    module BackIndex = Hashtbl.Make(Int)

    (* let record, backindex =
     *   let aux : type a index. (a,index)Goption.t -> (int->t->unit)*((int->t,index)Goption.t)
     *     = function
     *       | Goption.Some _ ->
     *         let backtable = BackIndex.create 5003 in
     *         BackIndex.add backtable,
     *         Goption.Some(BackIndex.find backtable)
     *       | Goption.None -> (fun _ _ -> ()),Goption.None
     *   in aux M.backindex *)

    let build a =
      (* let f = {reveal =  a; id = !unique; data = None} in *)
      (* try H.find table f *)
      try H.find table a
      with Not_found ->
        let id = !unique in
        let rec newf = G { reveal =  a; id ; data = lazy(Data.build newf)} in
        let G{data} = newf in
        let _ = Lazy.force data in
        incr unique;
        H.add table a newf;
        (* H.add table newf; *)
        (* record id newf; *)
        newf

    let clear() =
      unique := 0;
      H.clear table

  end

  module Init(Par: sig type t [@@deriving eq] val hash:t Hash.t end)
    = InitData(Par)(EmptyData)

end

module Make(M: Arg) = struct

  module N = struct
    type ('t,'a) t = 't M.t [@@deriving eq]
    let hash hrec _ = M.hash hrec
    let name = M.name
  end
  module TMP = MakePoly(N)

  type 'dh generic    = (unit,'dh) TMP.generic
  type 'dh g_revealed = (unit,'dh) TMP.g_revealed

  let reveal  = TMP.reveal
  let data    = TMP.data
  let noHCons = TMP.noHCons
    
  module InitData
      (Data: sig
         type t
         val build : (t*[`HCons]) generic -> t
       end)
    = TMP.InitData(struct type t = unit [@@deriving eq] let hash () = 5 end)(Data)

  module Init() = InitData(EmptyData)

end
