let length l =
  let rec iter acc = function
    | [] -> acc
    | _::t -> iter (acc+1) t
  in iter 0 l
;;

let rec append l1 l2 = match l1 with
  | [] -> l2
  | h::t -> h::(append t l2)
;;

let rec flatten = function
    | [] -> []
    | h::t -> append h (flatten t)
;;


let rec check c = function
  | [] -> false
  | h::t when h = c -> true
  | _::t -> check c t
;;

let rec remove c = function
  | [] -> []
  | h::t when h = c -> t
  | h::t -> h::remove c t
;;

let rec remove_all c = function
    | [] -> []
    | h::t when h = c -> remove_all c t
    | h::t -> h::remove_all c t

let rec list_uniq = function
    | [] -> []
    | h::t -> h::list_uniq (remove_all h t)
;;

let list_match l1 l2 = list_uniq (append l1 l2);;

(*Fonction utiles suplémentaires*)

let rec nth n l = match (n,l) with
  | (_, []) -> invalid_arg ((string_of_int n)^" is out of bound")
  | (0, h::t) -> h
  | (n, h::t) -> nth (n-1) t
;;

let rec map f = function
  | [] -> []
  | h::t -> f h::map f t
;;

let rec iter f = function
  | [] -> ()
  | h::t -> f h; iter f t
;;

let rec forall p = function
  | [] -> true
  | h::t -> p h && forall p t
;;

let rec range a b =
  if a = b then
    [a]
  else
    a::range (a+1) b
;;

let rec filter f = function
  | [] -> []
  | h::t when f h -> h::filter f t
  | _::t -> filter f t
;;

let occu e l =
  let rec iter acc = function
    | [] -> acc
    | h::t when e = h -> iter (acc+1) t
    | _::t -> iter acc t
  in iter 0 l
;;
