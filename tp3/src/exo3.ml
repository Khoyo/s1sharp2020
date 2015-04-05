
let find_missing l1 l2 =
  filter (function x -> x <> 0 && not (check x l1)) l2
;;

let grid_find grid x y =
  let line = extract_row grid y values
  and column = extract_column grid x values
  and square = extract_square grid ((x-1)/3 + ((y-1)/3*3) + 1)  values
  in find_missing column (find_missing line (find_missing square values))
;;

let extract_square_xy l y x = (* Doesn't flatten the square*)
  nth (y-1) (nth (x-1) l)
;;

let grid_get_val grid x y =
  let square = extract_square_xy grid ((x-1)/3 +1) ((y-1)/3 +1)  in
  nth ((x-1) mod 3) (nth ((y-1) mod 3) square)
;;

let grid_nsolve grid =
  let new_value x y =
    let value = grid_get_val grid x y in
    if value <> 0 then
      value
    else
      let res = grid_find grid x y in
      match res with
        | [] ->
            failwith ("Incorrect sudoku, no possibility in "^string_of_int x ^", "^string_of_int y)
        | [e] -> e
        |  _ -> 0
  in
  let rec iter_val x y = function
    | 3 -> []
    | acc -> new_value (x+acc) y::iter_val x y (acc+1)
  in
  let rec iter_sl x y = function
    | 3 -> []
    | acc -> iter_val x (y+acc) 0::iter_sl x y (acc+1)
  in
  let rec iter_sq x y = match x with
    | 10 -> []
    | x -> iter_sl x y 0::iter_sq (x+3) y
  in
  let rec iter_sql = function
    | 10 -> []
    | y -> iter_sq 1 y::iter_sql (y+3)
  in
    iter_sql 1;;

let rec solve grid =
  if grid_isfull grid then
    grid
  else
    solve (grid_nsolve grid)
;;

