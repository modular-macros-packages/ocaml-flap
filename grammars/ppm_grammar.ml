module T = struct
  type t = Grammar_tokens.Ppm.t = INT | P3
  $gen compare : t -> t -> int = fun x y -> Grammar_tokens.Ppm.compare x y
  $gen pp : Format.formatter -> t -> unit = fun fmt x -> Grammar_tokens.Ppm.pp fmt x
end

module G = Grammars_common.Common[T]
module P = G.P
module L = G.L
module C = Flap.Cd

$gen int : unit -> int P.t = fun () ->
  P.tok T.INT (fun s -> C.injv << int_of_string $(C.dyn s) >>)
$gen p3 : unit -> unit P.t = fun () -> P.tok T.P3 (fun _ -> C.injv << () >>)

$gen lexer : unit -> (Reex.t * P.rhs) list = fun () ->
  [ Reex.str "P3"                                        , P.Return T.P3;
    Reex.plus (L.digit ())                               , P.Return T.INT;
    Reex.(>>>) (Reex.chr '#') (Reex.star (Reex.not (Reex.chr '\n'))), P.Skip;
    Reex.chr '\n'                                        , P.Skip; (* TODO: record newline *)
    L.charset "\r\n \t"                                  , P.Skip;
    Reex.any                                             , P.Error "Unexpected character"
  ]

$gen ints : unit -> (int * int) P.t = fun () ->
  P.fix @@ fun ints ->
  P.(<|>) (P.eps (C.injv << (min_int, 0) >>))
    (P.($.) (P.(>>>) (int ()) ints)
       (fun p -> C.inj << let (i, (m,count)) = $(C.dyn p) in
                          (max i m, succ count) >>))

$gen exp : unit -> bool P.t = fun () ->
  P.($.) (P.(>>>) (P.(>>>) (P.(>>>) (P.(>>>) (p3 ()) (int ())) (int ())) (int ())) (ints ()))
    (fun p -> C.inj << let ((((_,w),h),max),(max',count)) = $(C.dyn p) in
                       (max' <= max) && (count = 3 * w * h) >>)

$gen code : unit -> (string -> bool) expr = fun () ->
  Result.get_ok (G.build "ppm" (exp ()) (lexer ()))
