
type 'a value = Atomic of 'a expr
              | Pair : 'a value * 'b value -> ('a * 'b) value

type 'a cd = Value of 'a value
           | LetComp : 'a expr * ('a expr -> 'b cd) -> 'b cd

type 'a t = 'a cd

macro rec fst : 'a 'b. ('a * 'b) cd -> 'a cd = fun (type a b) (c : (a * b) cd) : a cd ->
  match c with
  | Value (Atomic v) -> Value (Atomic << Stdlib.fst $v >>)
  | Value (Pair (x,_)) -> Value x
  | LetComp (e, k) -> LetComp (e, fun x -> fst (k x))

macro rec snd : 'a 'b. ('a * 'b) cd -> 'b cd = fun (type a b) (c : (a * b) cd) : b cd ->
  match c with
  | Value (Atomic v) -> Value (Atomic << Stdlib.snd $v >>)
  | Value (Pair (_,y)) -> Value y
  | LetComp (e, k) -> LetComp (e, fun x -> snd (k x))

macro rec pair : 'a 'b. 'a cd -> 'b cd -> ('a * 'b) cd =
  fun x y ->
  match x, y with
  | Value x, Value y -> Value (Pair (x, y))
  | LetComp (e, k), e2 -> LetComp (e, fun x -> pair (k x) e2)
  | Value x, LetComp (e, k) -> LetComp (e, fun y -> pair (Value x) (k y))

macro inj : 'a. 'a expr -> 'a cd = fun e -> LetComp (e, fun x -> Value (Atomic x))
macro injv : 'a. 'a expr -> 'a cd = fun v -> Value (Atomic v)
macro rec dyn : 'a. 'a cd -> 'a expr = fun (type a) (c : a cd) : a expr ->
  match c with
  | Value (Atomic c) -> c
  | Value (Pair (x, y)) -> << ($(dyn (Value x)), $(dyn (Value y))) >>
  | LetComp (e, k) -> << let x = $e in $(dyn (k <<x>>)) >>

macro rec let_ : 'a 'b. 'a cd -> ('a cd -> 'b cd) -> 'b cd =
  fun e k ->
  match e with
    Value _ as v -> k v
  | LetComp (e1, k1) -> LetComp (e1, fun x -> let_ (k1 x) k)

macro unit : unit -> unit cd = fun () -> Value (Atomic << () >>)
