  let rec word_to_morse l = match l with
    | [] -> []
    | t::q -> letter_to_morse t :: word_to_morse q;;
  let rec to_single_list l = match l with
    | [] -> []
    | [e] -> e
    | t::q -> append (append t [' ']) (to_single_list q);;
