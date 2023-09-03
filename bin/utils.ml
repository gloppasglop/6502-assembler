let int_of_hexstring s =
  if String.sub s 0 1 = "0"
  then int_of_string s
  else int_of_string ("0x" ^ String.sub s 1 (String.length s - 1))
;;

let int_of_octalstring s =
  if String.sub s 0 2 = "0o"
  then int_of_string s
  else int_of_string ("0o" ^ String.sub s 1 (String.length s - 1))
;;

let int_of_binarystring s =
  if String.sub s 0 2 = "0b"
  then int_of_string s
  else int_of_string ("0b" ^ String.sub s 1 (String.length s - 1))
;;
