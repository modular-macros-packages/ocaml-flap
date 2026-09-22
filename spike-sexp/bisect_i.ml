module T = struct
  type t = Spike_tokens.t = LP | RP | ATOM | QATOM
  $gen compare : t -> t -> int = fun x y -> Spike_tokens.compare x y
  $gen pp : Format.formatter -> t -> unit = fun fmt x -> Spike_tokens.pp fmt x
end

module G = Grammars_common.Common[T]
module P = G.P
module C = Flap.Cd
$gen lp : unit -> unit P.t = fun () -> P.tok T.LP (fun _ -> C.injv << () >>)
