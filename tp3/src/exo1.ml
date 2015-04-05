
let rec grid_make_rectangle x y =
  let rec make_null_list = function
    | 0 -> []
    | n -> 0::make_null_list (n-1)
  in match y with
    | 0 -> []
    | y -> make_null_list x ::grid_make_rectangle x (y-1)
;;

let grid_make_square c = grid_make_rectangle c c;;

let rec grid_make_rectangle_gen e x y = (* e est la valeur de l'element nul ;) *)
  let rec make_null_list e = function
    | 0 -> []
    | n -> e::make_null_list e (n-1)
  in match y with
    | 0 -> []
    | y -> make_null_list e x ::grid_make_rectangle_gen e x (y-1)
;;

let grid_make x =
  let x = int_of_float (sqrt (float_of_int x)) in
  print_int x; grid_make_rectangle_gen (grid_make_square x) x x;;



