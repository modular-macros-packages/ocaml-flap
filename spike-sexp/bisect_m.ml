let f x = if x > 0 then x else raise Stdlib.Exit

module T = struct
  type t = Spike_tokens.t = LP | RP | ATOM | QATOM
  $gen compare : t -> t -> int = fun x y -> Spike_tokens.compare x y
  $gen pp : Format.formatter -> t -> unit = fun fmt x -> Spike_tokens.pp fmt x
end

module G = Grammars_common.Common[T]
module P = G.P

$gen lp2 : unit -> unit P.t = fun () -> P.tok T.LP (fun _ -> Flap.Cd.injv << () >>)
