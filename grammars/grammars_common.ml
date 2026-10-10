
module Common $(Term : Flap.Sigs.TERM) =
struct
  module P =
  struct
    open Flap.Cd
    include Flap.Parse$(Term)
    $let option : 'a. 'a t -> 'a option t = fun e ->
      (e $ fun x -> let_ x @@ fun x -> injv << Option.Some $(dyn x) >>) <|> (eps (injv << Option.None >>))
    $let star : 'a. 'a t -> 'a list t = fun e ->
      fix @@ fun x -> (eps (injv << [] >>)
                       <|> (e >>> x $ fun p -> let_ p @@ fun p ->
                                               injv << $(dyn (fst p)) :: $(dyn (snd p)) >>))
    $let oneormore : 'a. 'a t -> 'a list t = fun e ->
      (e >>> star e) $ fun p -> let_ p @@ fun p -> injv << $(dyn (fst p)) :: $(dyn (snd p)) >>
  end

  module L =
  struct
        open Reex
    (* Various useful regex combinators *)
    $let charset : string -> Reex.t = fun s ->
      Seq.fold_left (fun s c -> s <|> chr c) empty (String.to_seq s)
    $let upper : Reex.t = range 'A' 'Z'
    $let lower : Reex.t = range 'a' 'z'
    $let digit : Reex.t = range '0' '9'
    $let alpha : Reex.t = upper <|> lower
    $let alnum : Reex.t = alpha <|> digit
    $let decimal : Reex.t = plus digit >>> opt (chr '.' >>> plus digit)
    $let complement : string -> Reex.t = fun s -> any <&> not (charset s)
    $let stringchar : Reex.t = complement "\"\\" <|> (chr '\\' >>> any)
    $let string : Reex.t = chr '"' >>> star stringchar >>> chr '"'

    $let (==>) : Reex.t -> Term.t -> Reex.t * P.rhs = fun ss v -> (ss, P.Return v)
    $let skip : Reex.t -> Reex.t * P.rhs = fun ss -> (ss, P.Skip)
    $let reject : Reex.t -> string -> Reex.t * P.rhs = fun ss msg -> (ss, P.Error msg)
    $let (!) : string -> Reex.t = regex
  end

  $let build : 'a. string -> 'a P.t -> (Reex.t * P.rhs) list -> ((string -> 'a) expr, string) result =
    fun name parser lexer ->
    let start_time = Sys.time () in
    let fused = P.compile lexer parser in
    let after_fusion = Sys.time () in
    Printf.fprintf stderr "[%s]: compilation time : %gms\n" name ((after_fusion -. start_time) *. 1000.);
    fused
end
