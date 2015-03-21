let rec is_morse l = match l with
  |[] -> true
  |t::q when t = '.' -> true && is_morse q
  |t::q when t = '-' -> true && is_morse q
  |_ -> false;;
(*3.2*)
  let letter_to_morse a = match a with
    'A'|'a' -> ['.'; '-']
  | 'B'|'b' -> ['-'; '.'; '.'; '.']
  | 'C'|'c' -> ['-'; '.'; '-'; '.']
  | 'D'|'d' -> ['-'; '.'; '.']
  | 'E'|'e' -> ['.']
  | 'F'|'f' -> ['.'; '.'; '-'; '.']
  | 'G'|'g' -> ['-'; '-'; '.']
  | 'H'|'h' -> ['.'; '.'; '.'; '.']
  | 'I'|'i' -> ['.'; '.']
  | 'J'|'j' -> ['.'; '-'; '-'; '-']
  | 'K'|'k' -> ['-'; '.'; '-']
  | 'L'|'l' -> ['.'; '-'; '.'; '.']
  | 'M'|'m' -> ['-'; '-']
  | 'N'|'n' -> ['-'; '.']
  | 'O'|'o' -> ['-'; '-'; '-']
  | 'P'|'p' -> ['.'; '-'; '-'; '.']
  | 'Q'|'q' -> ['-'; '-'; '.'; '-']
  | 'R'|'r' -> ['.'; '-'; '.']
  | 'S'|'s' -> ['.'; '.'; '.']
  | 'T'|'t' -> ['-']
  | 'U'|'u' -> ['.'; '.'; '-']
  | 'V'|'v' -> ['.'; '.'; '.'; '-']
  | 'W'|'w' -> ['.'; '-'; '-']
  | 'X'|'x' -> ['-'; '.'; '.'; '-']
  | 'Y'|'y' -> ['-'; '.'; '-'; '-']
  | 'Z'|'z' -> ['-'; '-'; '.'; '.']
  | '0' -> ['-'; '-'; '-'; '-'; '-']
  | '1' -> ['.'; '-'; '-'; '-'; '-']
  | '2' -> ['.'; '.'; '-'; '-'; '-']
  | '3' -> ['.'; '.'; '.'; '-'; '-']
  | '4' -> ['.'; '.'; '.'; '.'; '-']
  | '5' -> ['.'; '.'; '.'; '.'; '.']
  | '6' -> ['-'; '.'; '.'; '.'; '.']
  | '7' -> ['-'; '-'; '.'; '.'; '.']
  | '8' -> ['-'; '-'; '-'; '.'; '.']
  | '9' -> ['-'; '-'; '-'; '-'; '.']
  | _ -> failwith "lolk";;
