let probe x = x + 1

module T = struct
  type t = Spike_tokens.t = LP | RP | ATOM | QATOM
  macro compare : t -> t -> int = fun x y -> Spike_tokens.compare x y
  macro pp : Format.formatter -> t -> unit = fun fmt x -> Spike_tokens.pp fmt x
end

module G = Grammars_common.Common[T]
module P = G.P

macro lp2 : unit -> unit P.t = fun () -> P.tok T.LP (fun _ -> Flap.Cd.injv << () >>)
