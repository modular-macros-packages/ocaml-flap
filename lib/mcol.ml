
open Sigs

module Mset [E : ORDERED] :
sig
  type t
  macro empty : unit -> t
  macro is_empty : t -> bool
  macro mem : E.t -> t -> bool
  macro singleton : E.t -> t
  macro add : E.t -> t -> t
  macro of_list : E.t list -> t
  macro union : t -> t -> t
  macro inter : t -> t -> t
  macro equal : t -> t -> bool
  macro iter : (E.t -> unit) -> t -> unit
  macro fold : 'a. (E.t -> 'a -> 'a) -> t -> 'a -> 'a
  macro elements : t -> E.t list
end =
struct
  type t = E.t list  

  macro empty : unit -> t = fun () -> []
  macro is_empty : t -> bool = fun s -> s = []

  macro mem : E.t -> t -> bool = fun x s ->
    List.exists (fun y -> E.compare x y = 0) s

  macro singleton : E.t -> t = fun x -> [x]

  macro add : E.t -> t -> t = fun x s ->
    let rec go = function
      | [] -> [x]
      | y :: ys as l ->
        let c = E.compare x y in
        if c < 0 then x :: l else if c = 0 then l else y :: go ys
    in go s

  macro of_list : E.t list -> t = fun l -> List.fold_left (fun s x -> add x s) (empty ()) l

  macro union : t -> t -> t = fun l r ->
    let rec go l r = match l, r with
      | [], t | t, [] -> t
      | x :: xs, y :: ys ->
        let c = E.compare x y in
        if c < 0 then x :: go xs r
        else if c > 0 then y :: go l ys
        else x :: go xs ys
    in go l r

  macro inter : t -> t -> t = fun l r ->
    let rec go l r = match l, r with
      | [], _ | _, [] -> []
      | x :: xs, y :: ys ->
        let c = E.compare x y in
        if c < 0 then go xs r
        else if c > 0 then go l ys
        else x :: go xs ys
    in go l r

  macro equal : t -> t -> bool = fun l r ->
    let rec go l r = match l, r with
      | [], [] -> true
      | x :: xs, y :: ys -> E.compare x y = 0 && go xs ys
      | _ -> false
    in go l r

  macro iter : (E.t -> unit) -> t -> unit = fun f s -> List.iter f s
  macro fold : 'a. (E.t -> 'a -> 'a) -> t -> 'a -> 'a =
    fun f s acc -> List.fold_left (fun acc x -> f x acc) acc s
  macro elements : t -> E.t list = fun s -> s
end

module Mmap [K : ORDERED] :
sig
  type 'a t
  macro empty : 'a. unit -> 'a t
  macro singleton : 'a. K.t -> 'a -> 'a t
  macro add : 'a. K.t -> 'a -> 'a t -> 'a t
  macro find : 'a. K.t -> 'a t -> 'a
  macro find_opt : 'a. K.t -> 'a t -> 'a option
  macro iter : 'a. (K.t -> 'a -> unit) -> 'a t -> unit
  macro fold : 'a 'b. (K.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  macro map : 'a 'b. ('a -> 'b) -> 'a t -> 'b t
  macro mapi : 'a 'b. (K.t -> 'a -> 'b) -> 'a t -> 'b t
  macro union : 'a. (K.t -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
end =
struct
  type 'a t = (K.t * 'a) list  

  macro empty : 'a. unit -> 'a t = fun () -> []
  macro singleton : 'a. K.t -> 'a -> 'a t = fun k v -> [(k, v)]

  macro add : 'a. K.t -> 'a -> 'a t -> 'a t = fun k v m ->
    let rec go = function
      | [] -> [(k, v)]
      | ((k', _) as e) :: es as l ->
        let c = K.compare k k' in
        if c < 0 then (k, v) :: l
        else if c = 0 then (k, v) :: es
        else e :: go es
    in go m

  macro find_opt : 'a. K.t -> 'a t -> 'a option = fun k m ->
    let rec go = function
      | [] -> None
      | (k', v) :: es ->
        let c = K.compare k k' in
        if c = 0 then Some v else if c < 0 then None else go es
    in go m

  macro find : 'a. K.t -> 'a t -> 'a = fun k m ->
    match find_opt k m with Some v -> v | None -> raise Not_found

  macro iter : 'a. (K.t -> 'a -> unit) -> 'a t -> unit =
    fun f m -> List.iter (fun (k, v) -> f k v) m
  macro fold : 'a 'b. (K.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b =
    fun f m acc -> List.fold_left (fun acc (k, v) -> f k v acc) acc m
  macro map : 'a 'b. ('a -> 'b) -> 'a t -> 'b t =
    fun f m -> List.map (fun (k, v) -> (k, f v)) m
  macro mapi : 'a 'b. (K.t -> 'a -> 'b) -> 'a t -> 'b t =
    fun f m -> List.map (fun (k, v) -> (k, f k v)) m

  macro union : 'a. (K.t -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t =
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
