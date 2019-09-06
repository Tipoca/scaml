open Spotlib.Spot
open Asttypes
open Typedtree

module List = struct
  include List

  let rec mark_last = function
    | [] -> []
    | [x] -> [x,true]
    | x::xs -> (x,false)::mark_last xs
                 
  let rec last = function
    | [] -> None
    | [x] -> Some x
    | _::xs -> last xs
end

module Longident = struct
  include Longident

  let rec to_string = function
    | Lident s -> s
    | Ldot (lid, s) -> to_string lid ^ "." ^ s
    | Lapply (t1,t2) -> to_string t1 ^ "(" ^ to_string t2 ^ ")"
end

module Ident = struct
  include Ident

  let is_stdlib i = name i = "Stdlib" && persistent i
  let is_scaml i = name i = "SCaml" && persistent i

  let dummy = Ident.create "_"
end

module Path = struct
  include Path

  let rec xname = function
    | Pident id -> Ident.unique_name id
    | Pdot (t, s, n) -> Printf.sprintf "%s.%s/%d" (xname t) s n
    | Papply (t1,t2) -> xname t1 ^ "(" ^ xname t2 ^ ")"
                        
  let is_stdlib = function
    | Pdot (Pident id, s, n) when Ident.is_stdlib id -> Some (s, n)
    | _ -> None
      
  let is_scaml = function
    | Pdot (Pident id, s, n) when Ident.is_scaml id -> Some (s, n)
    | _ -> None

  let is_scaml_dot n = function
    | Pdot (Pident id, s, _) -> s = n
    | _ -> false
end
