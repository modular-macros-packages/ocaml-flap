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

macro lp2 : unit -> unit P.t = fun () -> P.tok T.LP (fun _ -> Flap.Cd.injv << () >>)
