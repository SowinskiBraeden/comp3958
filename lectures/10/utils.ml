open Angstrom

let is_digit = function | '0'..'9' -> true | _ -> false

let is_space = function | ' ' | '\t' -> true | _ -> false

let ws = take_while is_space

let ws1 = take_while1 is_space

let uint_str = take_while1 is_digit

let uint_num = uint_str >>| int_of_string

let sign =
  peek_char >>= function
  | Some '+' -> advance 1 >>| fun () -> "+"
  | Some '-' -> advance 1 >>| fun () -> "-"
  | Some c when is_digit c -> return "+"
  | _ -> fail "digit or sign expected"

let int_str =
  let* s = sign in
  let* n = uint_str in
  return (s ^ n)

let int_num = int_str >>| int_of_string

let dot =
  peek_char >>= function
  | Some '.' -> advance 1 >>| fun () -> true
  | _ -> return false

let float_str =
  let* n = int_str in
  let* is_dot = dot in
  if is_dot then 
    let* f = uint_str in return (n ^ "." ^ f)
  else
    return n

let float_num = float_str >>| float_of_string
