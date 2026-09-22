
open Sigs

module Mset [E : ORDERED] :
sig
  type t
  $gen empty : unit -> t
  $gen is_empty : t -> bool
  $gen mem : E.t -> t -> bool
  $gen singleton : E.t -> t
  $gen add : E.t -> t -> t
  $gen of_list : E.t list -> t
  $gen union : t -> t -> t
  $gen inter : t -> t -> t
  $gen equal : t -> t -> bool
  $gen iter : (E.t -> unit) -> t -> unit
  $gen fold : 'a. (E.t -> 'a -> 'a) -> t -> 'a -> 'a
  $gen elements : t -> E.t list
end =
struct
  type t = E.t list  

  $gen empty : unit -> t = fun () -> []
  $gen is_empty : t -> bool = fun s -> s = []

  $gen mem : E.t -> t -> bool = fun x s ->
    List.exists (fun y -> E.compare x y = 0) s

  $gen singleton : E.t -> t = fun x -> [x]

  $gen add : E.t -> t -> t = fun x s ->
    let rec go = function
      | [] -> [x]
      | y :: ys as l ->
        let c = E.compare x y in
        if c < 0 then x :: l else if c = 0 then l else y :: go ys
    in go s

  $gen of_list : E.t list -> t = fun l -> List.fold_left (fun s x -> add x s) (empty ()) l

  $gen union : t -> t -> t = fun l r ->
    let rec go l r = match l, r with
      | [], t | t, [] -> t
      | x :: xs, y :: ys ->
        let c = E.compare x y in
        if c < 0 then x :: go xs r
        else if c > 0 then y :: go l ys
        else x :: go xs ys
    in go l r

  $gen inter : t -> t -> t = fun l r ->
    let rec go l r = match l, r with
      | [], _ | _, [] -> []
      | x :: xs, y :: ys ->
        let c = E.compare x y in
        if c < 0 then go xs r
        else if c > 0 then go l ys
        else x :: go xs ys
    in go l r

  $gen equal : t -> t -> bool = fun l r ->
    let rec go l r = match l, r with
      | [], [] -> true
      | x :: xs, y :: ys -> E.compare x y = 0 && go xs ys
      | _ -> false
    in go l r

  $gen iter : (E.t -> unit) -> t -> unit = fun f s -> List.iter f s
  $gen fold : 'a. (E.t -> 'a -> 'a) -> t -> 'a -> 'a =
    fun f s acc -> List.fold_left (fun acc x -> f x acc) acc s
  $gen elements : t -> E.t list = fun s -> s
end

module Mmap [K : ORDERED] :
sig
  type 'a t
  $gen empty : 'a. unit -> 'a t
  $gen singleton : 'a. K.t -> 'a -> 'a t
  $gen add : 'a. K.t -> 'a -> 'a t -> 'a t
  $gen find : 'a. K.t -> 'a t -> 'a
  $gen find_opt : 'a. K.t -> 'a t -> 'a option
  $gen iter : 'a. (K.t -> 'a -> unit) -> 'a t -> unit
  $gen fold : 'a 'b. (K.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  $gen map : 'a 'b. ('a -> 'b) -> 'a t -> 'b t
  $gen mapi : 'a 'b. (K.t -> 'a -> 'b) -> 'a t -> 'b t
  $gen union : 'a. (K.t -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
end =
struct
  type 'a t = (K.t * 'a) list  

  $gen empty : 'a. unit -> 'a t = fun () -> []
  $gen singleton : 'a. K.t -> 'a -> 'a t = fun k v -> [(k, v)]

  $gen add : 'a. K.t -> 'a -> 'a t -> 'a t = fun k v m ->
    let rec go = function
      | [] -> [(k, v)]
      | ((k', _) as e) :: es as l ->
        let c = K.compare k k' in
        if c < 0 then (k, v) :: l
        else if c = 0 then (k, v) :: es
        else e :: go es
    in go m

  $gen find_opt : 'a. K.t -> 'a t -> 'a option = fun k m ->
    let rec go = function
      | [] -> None
      | (k', v) :: es ->
        let c = K.compare k k' in
        if c = 0 then Some v else if c < 0 then None else go es
    in go m

  $gen find : 'a. K.t -> 'a t -> 'a = fun k m ->
    match find_opt k m with Some v -> v | None -> raise Not_found

  $gen iter : 'a. (K.t -> 'a -> unit) -> 'a t -> unit =
    fun f m -> List.iter (fun (k, v) -> f k v) m
  $gen fold : 'a 'b. (K.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b =
    fun f m acc -> List.fold_left (fun acc (k, v) -> f k v acc) acc m
  $gen map : 'a 'b. ('a -> 'b) -> 'a t -> 'b t =
    fun f m -> List.map (fun (k, v) -> (k, f v)) m
  $gen mapi : 'a 'b. (K.t -> 'a -> 'b) -> 'a t -> 'b t =
    fun f m -> List.map (fun (k, v) -> (k, f k v)) m

  $gen union : 'a. (K.t -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t =
    fun f l r ->
    let rec go l r = match l, r with
      | [], t | t, [] -> t
      | ((k1, v1) as e1) :: l', ((k2, v2) as e2) :: r' ->
        let c = K.compare k1 k2 in
        if c < 0 then e1 :: go l' r
        else if c > 0 then e2 :: go l r'
        else (match f k1 v1 v2 with
              | Some v -> (k1, v) :: go l' r'
              | None -> go l' r')
    in go l r
end
