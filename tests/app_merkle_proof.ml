open SCaml

type node =
  | Hash of bytes
  | Value of bytes
  | Internal
  | Extender of bytes

type tree = node list

type command =
  | Node
  | Internal
  | Extender of bytes

let f = Crypto.blake2b

let hash = function
  | [], _, _ -> assert false
  | Node::cs, Hash h::ns, hs -> cs, ns, h::hs
  | Node::cs, Value bs::ns, hs -> cs, ns, f bs::hs
  | Node::cs, Internal::ns, hs -> Node::Node::Internal::cs, ns, hs
  | Node::cs, Extender seg::ns, hs -> Node::Extender seg::cs, ns, hs
  | Internal::cs, ns, h1::h2::hs -> cs, ns, f (Bytes.concat h1 h2)::hs
  | Extender seg::cs, ns, h::hs -> cs, ns, Bytes.concat h seg::hs
  | Node::_, [], _ -> assert false
  | Internal::_, _, _ -> assert false
  | Extender _::_, _, _ -> assert false

let hash ns =
  Loop.left (fun x ->
      let x = hash x in
      match x with
      | [], [], [h] -> Right h
      | _ -> Left x) ([Node], ns, [])

let [@entry] check (ns, h) () =
  assert (hash ns = h);
  [], ()
