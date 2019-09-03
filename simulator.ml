type nonrec string = string
type int = Z.t
type nat = Nat of Z.t [@@unboxed]
type nonrec bytes = bytes
type nonrec bool = bool
type nonrec unit = unit
type nonrec 'a list = 'a list
type ('a, 'b) pair = 'a * 'b
type nonrec 'a option = 'a option
type ('a, 'b) or_ = Left of 'a | Right of 'b

  
(*
module Map : sig
  type ('k, 'v) t
  val empty : unit -> ('k, 'v) t
(*
  val get : 'k -> ('k, 'v) t -> 'v option
  val mem : 'k -> ('k ,'v) t -> bool
  val update : 'k -> 'v option -> ('k, 'v) t -> ('k, 'v) t
  val map : ('k -> 'v -> 'w) -> ('k, 'v) t -> ('k, 'w) t
  val fold : ('k -> 'v -> 'a -> 'a) -> ('k, 'v) t -> 'a -> 'a
  val size : ('k, 'v) t -> nat
*)
end = struct 
  module type S = sig
    include Map.S
    type t0
    type v
  end

  type ('k, 'v, 'm) t1 = 
    { m : (module S with type t0 = 'm and type key = 'k and type v = 'v);
      s : 'm }
  type ('k, 'v) t = E : ('k, 'v, 'm) t1 -> ('k, 'v) t

  let empty (type k) (type v) () : (k, v) t =
    let module M = struct
      include Map.Make(struct
          type t = k
          let compare = compare
        end)
      type t0 = v t
      type nonrec v = v
    end in
    E { m = (module M) ; s = M.empty }
      
  let get (type k) (type v) k (E { m ; s } : (k, v) t) =
    let module M = (val m) in
    M.find_opt k s
end
      *)
                    
