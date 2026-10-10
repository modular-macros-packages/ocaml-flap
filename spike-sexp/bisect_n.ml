let f () = Buffer.create 16

module T = struct
  type t = Spike_tokens.t = LP | RP | ATOM | QATOM
  $let compare : t -> t -> int = fun x y -> Spike_tokens.compare x y
  $let pp : Format.formatter -> t -> unit = fun fmt x -> Spike_tokens.pp fmt x
end

module G = Grammars_common.Common$(T)
module P = G.P

$let lp2 : unit -> unit P.t = fun () -> P.tok T.LP (fun _ -> Flap.Cd.injv << () >>)
