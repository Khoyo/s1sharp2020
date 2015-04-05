
let grid_sample =
  [
   [
    [[5; 3; 0]; [6; 0; 0]; [0; 9; 8]];
    [[0; 7; 0]; [1; 9; 5]; [0; 0; 0]];
    [[0; 0; 0]; [0; 0; 0]; [0; 6; 0]]
   ];
   [
    [[8; 0; 0]; [4; 0; 0]; [7; 0; 0]];
    [[0; 6; 0]; [8; 0; 3]; [0; 2; 0]];
    [[0; 0; 3]; [0; 0; 1]; [0; 0; 6]]
   ];
   [
    [[0; 6; 0]; [0; 0; 0]; [0; 0; 0]];
    [[0; 0; 0]; [4; 1; 9]; [0; 8; 0]];
    [[2; 8; 0]; [0; 0; 5]; [0; 7; 9]]
   ]
  ];;

let values = [1; 2; 3; 4; 5; 6; 7; 8; 9]

let extract_square l n values = match n with
  | n when n<1 || n>9 -> invalid_arg ("Square " ^ string_of_int n ^ "is Out of bound")
  | n -> flatten (nth ((n-1) mod 3) (nth ((n-1)/3) l))
;;

let extract_row l n values =
  let square_row = nth ((n-1)/3) l in
  flatten (map (nth ((n-1) mod 3)) square_row)
;;

let extract_column l n values =
  let column_from_square n s = map (nth n) s
  and square_from_row n r = nth n r
  in let squares = map (square_from_row ((n-1)/3)) l
  in flatten (map (column_from_square ((n-1) mod 3)) squares)
;;

let grid_print l =
  let print_line = iter (function x -> print_int x; print_char ' ') in
  iter (function n -> print_line (extract_row l n values); print_newline ()) (range 1 9)
;;

let list_validate l1 l2 = forall (function x -> (x = 0 || occu x l1 <= 1 && occu x l2 > 0 )) l1;;

let grid_validate grid values =
  let lines_validate = forall (function x -> list_validate (extract_row grid x values) values) (range 1 9)
  and square_validate = forall (forall ( (function s -> list_validate (flatten s) values))) grid
  and column_validate = forall (function x -> list_validate (extract_column grid x values) values) (range 1 9)
  in square_validate && lines_validate && column_validate
;;

let grid_isfull = forall (forall (forall (forall (function x -> x <> 0))));;

