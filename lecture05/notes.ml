(**** RECORDS ****)
type student = {id: string; name: string; gpa: float};;

let s1 = {id = "a12345678"; name = "homer simpson"; gpa = 25.5};;
let make_student id name gpa = { id = id; name = name; gpa = gpa };;
let s2 = make_student "a000000" "monty burns" 99.9;;

(* defining a function to return the name field of a record *)
let name {id = id; name = name; gpa = gpa} = name;;
let name {id; name; gpa} = name;;
let name {name} = name;;
let name s = s.name;;

name s2;;

type instructor = {id: string; name: string; salary: float};;
let i1 = {id = "a777777"; name = "lisa simpson"; salary = 100000.};;

(* name cannot be applied to constructor because name was defined before instructor
   and it deferred from student *)
(* name i1;; *)

let id x = x.id;;

(* id s1;; <-- this doesnt work because id infers from the latest defined, such as instructor *)
id i1;;

type isntr = Instructor of {id: string; name: string; salary: float};;
let i1 = Instructor {id = "a777777"; name = "lisa simpson"; salary = 100000.};;

let s1 = make_student "a123456678" "homer simpson" 25.5;;
let s2 = s1;; (* s2 is its own student it just shares the same data *)
let s2 = {s1 with id = "a22222222"; name = "bart simpson"} (* this allows us to copy data and modify selected data *)

(**** MUTABILITY - you can mark fields as mutable ****)
type student = {id: string; name: string; mutable gpa: float};;
let make_student id name gpa = { id = id; name = name; gpa = gpa };;
let s1 = make_student "a123456678" "homer simpson" 25.5;;
let s2 = s1;; (* refer to the same thing *)

s2;;

(* update field for s1 *)
s1.gpa <- 15.5;

(* shows changes applied to s1 since s2 refers to the same data *)
s2;;

(**** REFERENCES & DEREFEREMCES ****)
let x = ref 1;;
!x;;     (* deref *)
x := 2;; (* update val of references *)
x;;
!x;;

let y = x;;
y;;
x;;
incr x;; (* y is the same as x (ref to int) so updating x also shows in y *)
x;;
y;;

let x = ref (Some 1);;
let y = ref None;;
(* val y : '_wek1 option ref = {contents = None} *)
(* curremtly the contents of y are weakly polymorphic
  because it can be any type. the type inference cannot
  deduce the value of the optional *)
y := Some 2;;
y;;
(* - : int option ref = {contents = Some 1} *)

(**** ARRAYS - arrays are mutable ****)
[|1;2;3;3;4;5|];; (* arrays have vertical bars unlike lists *)
let a = [|1;2;3|];;
a.(0);; (* get first element *);;
a.(1);;
a.(2);;
(* a.(3);; (* Invalid_argument excetption *) *)

a.(0) <- -1;;
a;;

(**** FOR / WHILE LOOPS ****)
(* for loops on arrays... yikes *)
Array.length a;;
for i = 0 to Array.length a - 1 do
  a.(i) <- 2 * a.(i)
done;;

a;;

(* you can go in reverse also using downto *)
for i = Array.length a - 1 downto 0 do
  a.(i) <- 2 * a.(i)
done;;

(* while loops also... *)
let i = ref 0;;
while !i < Array.length a do
  (* adding semi-colon to end of expression means
     this is a garbage value, throw it away so we
     dont return unit() type early *)
  a.(!i) <- - !i;
  incr i
done;;

a;;

(**** IGNORE ****)
(* x will be 2 and the expression 1;
   is ignored because of the semi-colon *)
let x = ignore 1; 2;;

(**** EXCEPTIONS ****)
(* defining exceptions and rasing *)
(* exception is a variant type and the
   number of variants can be extended *)
(* exception Hell;; *)
(* raise Hell;;
Failure "hell";; *)

(* exceptions can have parameters *)
exception Hell of int;;
Hell 1;;
Hell 2;;

(* List.hd [];; (* raises an exception *) *)

(* you can pattern match exceptions with try-with *)
try
  List.hd []
with
| Failure _ -> -1;;

try
  List.hd []
with
| _ -> -1;;


(**** IO ****)
print_string "hello world\n";;
print_endline "hello world";;
print_int 123;;
print_float 123.4;;
print_newline ();;
Printf.printf "%5d\n" 123;;
Printf.printf "%05d %s\n" 123 "hello";;
Printf.eprintf "%05d %s\n" 123 "hello";; (* print to standard error channel *)

let x = read_line ();;

(* read in int - add space at the front, it matches any amount of leading whitespace *)
Scanf.scanf " %d" (fun x -> x)

(* open file *)
(* open_in "filename";; *)
let ic = open_in "output";;
input_line ic;;

