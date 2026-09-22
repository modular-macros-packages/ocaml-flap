type probe_t = P0 | P1 of int

let probe_fn (x : int) = x + 1

module T = struct
  type t = Spike_tokens.t = LP | RP | ATOM | QATOM
  $gen compare : t -> t -> int = fun x y -> Spike_tokens.compare x y
  $gen pp : Format.formatter -> t -> unit = fun fmt x -> Spike_tokens.pp fmt x
end

module G = Grammars_common.Common[T]

$gen trivial : unit -> unit G.P.t = fun () ->
  G.P.tok T.LP (fun _ -> Flap.Cd.injv << () >>)
