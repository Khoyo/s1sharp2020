let rec are_equal l1 l2 = match (l1, l2) with
  |([], [])->true
  |([], l2)->false
  |(l1, [])->false
  |(h1::t1, h2::t2) -> h1=h2 && are_equal t1 t2;;
let rec append l1 l2 = match l1 with
  | [] -> l2
  | t::q -> t::(append q l2);;
let reverse l =
  let rec iter acc l =  match l with
    | []-> acc
    |t::q -> aux (t::acc) q
  in aux [] l ;;
let rec print_list = function 
   | [] -> ()
   | t::q -> (print_char t; print list q);;
