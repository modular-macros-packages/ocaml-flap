
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
  $let compare : t -> t -> int = fun x y -> Spike_tokens.compare x y
  $let pp : Format.formatter -> t -> unit = fun fmt x -> Spike_tokens.pp fmt x
end

module G = Grammars_common.Common$(T)
module P = G.P
module C = Flap.Cd
$let lp : unit -> unit P.t = fun () -> P.tok T.LP (fun _ -> C.injv << () >>)
$let rp : unit -> unit P.t = fun () -> P.tok T.RP (fun _ -> C.injv << () >>)
$let lexer : unit -> (Reex.t * P.rhs) list = fun () ->
  [ Reex.plus (G.L.complement "() \t\n\r\";#|\\"), P.Return T.ATOM;
    Reex.(>>>) (Reex.chr '"')
      (Reex.(>>>)
         (Reex.star
            (Reex.(<|>)
               (G.L.complement "\"\\")
               (Reex.(>>>) (Reex.chr '\\') Reex.any)))
         (Reex.chr '"')),
    P.Return T.QATOM;
    G.L.charset " \t\n\r", P.Skip;
    Reex.chr '(', P.Return T.LP;
    Reex.chr ')', P.Return T.RP;
  ]
