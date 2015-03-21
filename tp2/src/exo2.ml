  let rec word_to_morse l = match l with
    | [] -> []
    | t::q -> letter_to_morse t :: word_to_morse q;;
  let rec to_single_list l = match l with
    | [] -> []
    | [e] -> e
    | t::q -> append (append t [' ']) (to_single_list q);;
  let print l =
    let l = to_single_list l in
      let rec display l = match l with
	    |[] -> print_newline ()
	    |t::q -> print_char t;
	         display q in display q ;;
