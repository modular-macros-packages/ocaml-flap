module T = struct
  type t = Spike_tokens.t = LP | RP | ATOM | QATOM
  $let compare : t -> t -> int = fun x y -> Spike_tokens.compare x y
  $let pp : Format.formatter -> t -> unit = fun fmt x -> Spike_tokens.pp fmt x
end

module G = Grammars_common.Common$(T)
module P = G.P
module C = Flap.Cd
$let lp : unit -> unit P.t = fun () -> P.tok T.LP (fun _ -> C.injv << () >>)

$let wide_lexer : unit -> (Reex.t * P.rhs) list = fun () ->
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

$let probe_code : unit -> (string -> int) expr = fun () ->
  Result.get_ok
    (G.build "wide_probe"
       (P.( $ ) (P.tok T.ATOM (fun _ -> Flap.Cd.injv << 0 >>)) (fun x -> x))
       (wide_lexer ()))
