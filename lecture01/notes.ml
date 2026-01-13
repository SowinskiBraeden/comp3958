(*
  COMPILE = ocamlc -o outfile infile.ml

  UTOP = #use "filename.ml";;
*)

(*
  Ocaml is thankfully garbage collected

  Expressions must end with double semi colon
  e.g. let name = "Bread";;

  expressions can be evaluated to a value

  ocaml is a statically typed and strongly typed language
  meaning that at compile time the type of a variable will
  be known. The compiler uses type inference to determine the
  type of a variable that you dont need to specify;

  e.g.
*)

(* string type *)
"Hello World";;

(* character type *)
'A';;

(* integer type *)
50;;

(* floating type *)
20.3;;

(*
  OPERATIONS

  you have basic operations such as
  +, -, *, /, that works with integer types

  to do these operations with floating point
  numbers you need to use the following

  +., -., *., /., notice the "." after the operator

  -----------------------------------------------

  MODULE does not use the % sign

  use "mod" e.g.

  3 mod 2;; - : int = 1 (* evaluates to 1 *)

  ----------------------------------------------

  NOT uses the word "not" instead of "!" e.g.

  not (1 < 2);; - : bool = false
*)

(*
  COMPARATORS

  standard and, or comparisons e.g.

  a && b, a || b

  you can use >, <, but the comparator uses a
  single equal sign. e.g.

  1. = 2.;; - : bool = false (* evaluates to false *)

  -----------------------------------------------

  NOT EQUALS

  to check for inequality use "<>", e.g.

  1. <> 2.;; - bool = true (* evaluates to true since 1. and 2. are not equal *)
*)

(*
  STRING CONCATONATION

  use the "^" (carrot operator) to concat strings e.g.
*)
"Braeden" ^ " " ^ "Sowinski"

(*
  FUNCTIONS

  define a function square that takes in a
  parameter x and returns x * x
*)
let square x = x * x;;
square 5;;

(* evaluates to 6 *)
let x = 1 in x + 5;;

let add x y = x + y;;

(*
  arrows are right associative

    name : input -> input -> return type
  val add : int -> int -> int = <fun>

  so this can be translated to int -> (int -> int)

  this means that every function essentially takes in 1 argument
  that then returns another functoin that takes in another argument
*)

(* valid *)
add 1 2;;

(* valid *)
(add 1) 2;;

(* invalid *)
(*add (1 2);;*)

(*
  Ocaml is functional and there are no loops
  such as for or while,

  all variables are immutable
*)

let x = 1;;
let x = 2;;
(*
x = 3;; (* cannot reassign, this evaluates as a comparison
          therefore variables are immutable *)
*)
(*
  RECURSION

  recursion is related to mathematical induction,
  you typically have a proposition, e.g. P, where we
  know P(1) is true, and we assume P(k) is true
  where we can proove that P(k + 1) is true

  to declare a recursive function you need "rec"
*)
let rec factorial n = if n = 0 then 1 else n * factorial (n - 1);;
let r = (factorial 5);;
print_int r;;
print_endline "";;

(*
  tail-recursive

  a recursive function is tail-recursive if the recursive
  call is the last thing we do, for example, factorial is not
  tail-recursive, because we need to multiply n to the recursive call.

  non tail-recursive functions are bad since there is potential to bloew
  through the stack.

  lets consider factorial 3 = 3 * fact 2
                            = 3 * (2 * fact 1)
                            = 3 * (2 * (1 * fact 0))
                            = 3 * (2 * 1)
                            = 3 * 2
                            = 6

  we can see that this is not tail recursive as we need to store a value for
  each part of the iteration, we can solve this by rewriting the function a little
  using an accumulator
*)
let rec fact n acc = if n = 0 then acc else fact (n - 1) (n * acc);;

(* we can have primes, e.g. function f, and function f prime or f' *)

let factorial' n = fact n 1;;
let r = (factorial 5);;
print_int r;;
print_endline "";;

let factorial' = fact 1;;

(*
  consider the tail-recursive version of factorial, fact'

  the signature is fact' n acc

  fact' 1 3 = fact' 3 2
            = fact' 6 1
            = fact' 6 0

  as you can see, the stack would not grow

  lets combine the functoins into a single one
  with nesting
*)
(* final tail-recursive version *)
let fact n =
  let rec fact' acc i =
    if i = 0 then acc
    else fact' (i * acc) (i - 1)
  in
  fact' 1 n;; (* automatically sets the accumulator to 1 *)

let r = fact 5;;
print_int r;;
print_endline "";;

(* example of a documentation comment *)

(** [gcd a b] returns the greatest common divisor of [a] and [b]
  * Requires:[a > 0] and [b > 0]
  *)
let rec gcd a b =
  if a mod b = 0 then b
  else gcd b (a mod b);;
(*
let r = gcd 12 18;;
print_int r;;
print_endline "";; *)

let rec fib n =
  if n = 0 then 0
  else if n = 1 then 1
  else fib (n - 1) + fib (n - 2);;

let r = fib 10;;
print_int r;;
print_endline "";;

(* tail-recursive version *)
let rec fib n =
  let rec fib' i a b =
    if i = n then a
    else fib' (i + 1) b (a + b)
  in
  fib' 0 0 1;;

let r = fib 10;;
print_int r;;
print_endline "";;

(*
  MUTALLY-RECURSIVE FUNCTION
*)
let rec even n =
  if n = 0 then true
  else odd (n - 1)
and odd n =
  if n = 0 then false
  else even (n - 1);;

(*
  useful stuff

  float_of_int 4
  int_of_string "123"
*)

let square_root x =
  let good_enough y = abs_float (x -. y *. y) < 0.00000000001 in
  let rec aux y =
    if good_enough y then y
    else aux (0.5 *. (y +. x /. y))
  in
  aux 1.;;

print_float (square_root 2.);;
print_endline "";;

(*
  Consider this

  create an exponential function
  where it calculates e^x

  using recursion
*)
