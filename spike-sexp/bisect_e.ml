
type point = { px : int; py : int; tags : string list }
type itree = ILeaf | INode of itree * int * itree

let unescape (raw : string) : string =
  let n = String.length raw in
  let b = Buffer.create n in
  let i = ref 1 in
  while !i < n - 1 do
    (match raw.[!i] with
     | '\\' ->
       if !i + 1 >= n - 1 then raise Exit;
       (match raw.[!i + 1] with
        | '"' -> Buffer.add_char b '"'
        | '\\' -> Buffer.add_char b '\\'
        | 'n' -> Buffer.add_char b '\n'
        | 't' -> Buffer.add_char b '\t'
        | 'r' -> Buffer.add_char b '\r'
        | 'b' -> Buffer.add_char b '\b'
        | _ -> raise Exit);
       i := !i + 2
     | c ->
       Buffer.add_char b c;
       incr i)
  done;
  Buffer.contents b

module T = struct
  type t = Spike_tokens.t = LP | RP | ATOM | QATOM
  macro compare : t -> t -> int = fun x y -> Spike_tokens.compare x y
  macro pp : Format.formatter -> t -> unit = fun fmt x -> Spike_tokens.pp fmt x
end

module G = Grammars_common.Common[T]
module P = G.P
module C = Flap.Cd
macro lp : unit -> unit P.t = fun () -> P.tok T.LP (fun _ -> C.injv << () >>)
macro rp : unit -> unit P.t = fun () -> P.tok T.RP (fun _ -> C.injv << () >>)

macro key : string -> bool P.t = fun name ->
  P.tok T.ATOM (fun s ->
    C.let_ s (fun s ->
      C.injv << String.equal $(C.dyn s) $(Expr.string name) >>))

macro int_atom : unit -> int P.t = fun () ->
  P.tok T.ATOM (fun s ->
    C.let_ s (fun s ->
      C.injv << try int_of_string $(C.dyn s) with _ -> raise Stdlib.Exit >>))
macro kv : 'a. string -> 'a P.t -> 'a P.t = fun name value_g ->
  P.($.)
    (P.(>>>) (P.(>>>) (P.(>>>) (lp ()) (key name)) value_g) (rp ()))
    (fun p ->
      C.let_ p (fun p ->
        C.injv
          << if $(C.dyn (C.snd (C.fst (C.fst p))))
             then $(C.dyn (C.snd (C.fst p)))
             else raise Stdlib.Exit >>))
