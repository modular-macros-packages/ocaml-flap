
module Common [Term : Flap.Sigs.TERM] =
struct
  module P0 = Flap.Parse[Term]
  module P =
  struct
    include P0
    $gen option : 'a. 'a P0.t -> 'a option P0.t = fun e ->
      P0.(<|>)
        (P0.($.) e (fun x -> Flap.Cd.let_ x (fun x -> Flap.Cd.injv << Option.Some $(Flap.Cd.dyn x) >>)))
        (P0.eps (Flap.Cd.injv << Option.None >>))
    $gen star : 'a. 'a P0.t -> 'a list P0.t = fun e ->
      P0.fix (fun x ->
        P0.(<|>) (P0.eps (Flap.Cd.injv << [] >>))
          (P0.($.) (P0.(>>>) e x)
             (fun p -> Flap.Cd.let_ p (fun p ->
                Flap.Cd.injv << $(Flap.Cd.dyn (Flap.Cd.fst p)) :: $(Flap.Cd.dyn (Flap.Cd.snd p)) >>))))
    $gen oneormore : 'a. 'a P0.t -> 'a list P0.t = fun e ->
      P0.($.) (P0.(>>>) e (star e))
        (fun p -> Flap.Cd.let_ p (fun p ->
           Flap.Cd.injv << $(Flap.Cd.dyn (Flap.Cd.fst p)) :: $(Flap.Cd.dyn (Flap.Cd.snd p)) >>))
  end

  module L =
  struct
        $gen charset : string -> Reex.t = fun s ->
      Seq.fold_left (fun s c -> Reex.(<|>) s (Reex.chr c)) Reex.empty (String.to_seq s)
    $gen upper : unit -> Reex.t = fun () -> Reex.range 'A' 'Z'
    $gen lower : unit -> Reex.t = fun () -> Reex.range 'a' 'z'
    $gen digit : unit -> Reex.t = fun () -> Reex.range '0' '9'
    $gen alpha : unit -> Reex.t = fun () -> Reex.(<|>) (upper ()) (lower ())
    $gen alnum : unit -> Reex.t = fun () -> Reex.(<|>) (alpha ()) (digit ())
    $gen decimal : unit -> Reex.t = fun () ->
      Reex.(>>>) (Reex.plus (digit ())) (Reex.opt (Reex.(>>>) (Reex.chr '.') (Reex.plus (digit ()))))
    $gen complement : string -> Reex.t = fun s -> Reex.(<&>) Reex.any (Reex.not (charset s))
    $gen stringchar : unit -> Reex.t = fun () ->
      Reex.(<|>) (complement "\"\\") (Reex.(>>>) (Reex.chr '\\') Reex.any)
    $gen string : unit -> Reex.t = fun () ->
      Reex.(>>>) (Reex.chr '"') (Reex.(>>>) (Reex.star (stringchar ())) (Reex.chr '"'))

    $gen (==>) : Reex.t -> Term.t -> Reex.t * P0.rhs = fun ss v -> (ss, P0.Return v)
    $gen skip : Reex.t -> Reex.t * P0.rhs = fun ss -> (ss, P0.Skip)
    $gen reject : Reex.t -> string -> Reex.t * P0.rhs = fun ss msg -> (ss, P0.Error msg)
    $gen (!) : string -> Reex.t = fun s -> Reex.regex s
  end

  $gen build : 'a. string -> 'a P0.t -> (Reex.t * P0.rhs) list -> ((string -> 'a) expr, string) result =
    fun name parser_ lexer ->
    let start_time = Sys.time () in
    let fused = P0.compile lexer parser_ in
    let after_fusion = Sys.time () in
    Printf.fprintf stderr "[%s]: compilation time : %gms\n" name ((after_fusion -. start_time) *. 1000.);
    fused
end
