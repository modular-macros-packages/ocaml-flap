module T = struct
  type t = Grammar_tokens.Ppm.t = INT | P3
  $let compare : t -> t -> int = fun x y -> Grammar_tokens.Ppm.compare x y
  $let pp : Format.formatter -> t -> unit = fun fmt x -> Grammar_tokens.Ppm.pp fmt x
end

module G = Grammars_common.Common$(T)
open G
open Flap.Cd

$let int : int P.t = P.tok T.INT @@ fun s -> injv << int_of_string $(dyn s) >>
$let p3 : unit P.t = P.tok T.P3 @@ fun _ -> injv << () >>

$let lexer : (Reex.t * P.rhs) list =
  Reex.(
  L.[ str "P3"                          , P.Return P3;
      plus digit                        , P.Return INT;
      chr '#' >>> star (not (chr '\n')) , P.Skip;
      chr '\n'                          , P.Skip; (* TODO: record newline *)
      charset "\r\n \t"                 , P.Skip;
      any                               , P.Error "Unexpected character"
  ])

$let ints : (int * int) P.t =
           P.(fix @@ fun ints ->
                     (eps (injv << (min_int, 0) >>))
                     <|>
                     (int >>> ints $ fun p -> inj << let (i, (m,count)) = $(dyn p) in
                                                 (max i m, succ count) >>))

$let exp : bool P.t =
          P.((p3 >>> int >>> int >>> int >>> ints) $
               fun p -> inj << let ((((_,w),h),max),(max',count)) = $(dyn p) in
                           (max' <= max) && (count = 3 * w * h) >>)

$let code : unit -> (string -> bool) expr = fun () ->
  Result.get_ok (build "ppm" exp lexer)
