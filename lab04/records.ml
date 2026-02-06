(** [record] of a person with a first name, last name, and score *)
type record = {firstname: string; lastname: string; score: int};;

(** [new_records f l s] creates a new record with the given firstname [f],
  * lastname [l] and score [s] *)
let new_record f l s = {firstname = f; lastname = l; score = s};;

(**/**)
let test_new_record () =
  assert (new_record "a" "b" 1 = {firstname = "a"; lastname = "b"; score = 1});
  assert (new_record "b" "a" 2 = {firstname = "b"; lastname = "a"; score = 2});
  assert (new_record "c" "d" 3 = {firstname = "c"; lastname = "d"; score = 3})
(**/**)

(** [print_records r] takes a list of records [r] and prints
  * each record neatly to the console *)
let rec print_records r =
  match r with
  | [] -> ()
  | x :: xs -> Printf.printf "%3d %s %s\n" x.score x.lastname x.firstname; print_records xs;;

(** [records_insert x l] inserts a record element [x] into
  * list of records [l]
  * Requires: [l] is in ascending order. *)
let rec records_insert x l =
  match l with
  | y :: ys when x.score < y.score -> y :: records_insert x ys
  | _ -> x :: l

(**/**)
let test_records_insert () =
  assert (records_insert
    {firstname = "a"; lastname = "b"; score = 1} [] =
    [{firstname = "a"; lastname = "b"; score = 1}]
  );
  assert (records_insert
    {firstname = "a"; lastname = "b"; score = 1}
    [{firstname = "c"; lastname = "d"; score = 2}] =
  [
    {firstname = "c"; lastname = "d"; score = 2};
    {firstname = "a"; lastname = "b"; score = 1}
  ]);
  assert (records_insert
    {firstname = "e"; lastname = "f"; score = 2}
    [
      {firstname = "c"; lastname = "d"; score = 3};
      {firstname = "a"; lastname = "b"; score = 1}
    ] =
    [
      {firstname = "c"; lastname = "d"; score = 3};
      {firstname = "e"; lastname = "f"; score = 2};
      {firstname = "a"; lastname = "b"; score = 1}
    ])
(**/**)

(** [records_insertion_sort l] sorts list of records [l] in ascending order *)
let rec sort_records l =
  match l with
  | [] -> []
  | x :: xs -> records_insert x (sort_records xs)

(**/**)
let test_sort_records () =
  assert (sort_records [] = []);
  assert (sort_records
    [{firstname = "a"; lastname = "b"; score = 1}] =
    [{firstname = "a"; lastname = "b"; score = 1}]);

  assert (sort_records [
    {firstname = "c"; lastname = "d"; score = 3};
    {firstname = "a"; lastname = "b"; score = 1};
    {firstname = "e"; lastname = "f"; score = 2};
  ] = [
    {firstname = "c"; lastname = "d"; score = 3};
    {firstname = "e"; lastname = "f"; score = 2};
    {firstname = "a"; lastname = "b"; score = 1};
  ])
(**/**)

(** [parse l] takes in a line [l] and parses the line to extract
  * firstname, lastname, and score, returning Some record if successful
  * or None if the input line [l] contains bad data *)
let rec parse l =
  try
    Scanf.sscanf l " %s %s %s" (fun f l s ->
      match int_of_string_opt s with
      | Some v when v >= 0 && v <= 100 -> Some {firstname = f; lastname = l; score = v}
      | _ -> None
    )
  with
  | Scanf.Scan_failure _ -> None

(**/**)
let test_parse () =
  assert (parse "" = None);
  assert (parse "Bart simpson 5abc" = None);
  assert (parse "Lisa simpson 130" = None);
  assert (parse "Homer Simpson -5" = None);
  assert (parse "Homer Simpson 5" =
    Some {firstname = "Homer"; lastname = "Simpson"; score = 5});
  assert (parse "Homer Simpson 5 blah blah blah" =
    Some {firstname = "Homer"; lastname = "Simpson"; score = 5})
(**/**)

(** [read_file acc ic] takes in an accumulator [acc] and an input channel [ic]
  * and oterates over it input channel [ic] to parse each line and store the
  * parsed result into the accumulator [acc] *)
let rec read_file acc ic =
  try
    match parse @@ input_line ic with
    | None -> read_file acc ic
    | Some v -> read_file (v :: acc) ic
  with
  | End_of_file -> close_in ic; acc

(**/**)
let test_read_file () =
  assert (read_file [] @@ open_in "data.txt" = [
    {firstname = "gary"; lastname = "chalmers"; score = 5};
    {firstname = "waylon"; lastname = "smithers"; score = 100};
    {firstname = "homer"; lastname = "simpson"; score = 25};
  ]);
  assert (read_file [] @@ open_in "data1.txt" = [
    {firstname = "gary"; lastname = "chalmers"; score = 5};
    {firstname = "waylon"; lastname = "smithers"; score = 100};
    {firstname = "ned"; lastname = "flanders"; score = 12};
    {firstname = "homer"; lastname = "simpson"; score = 25};
  ]);
  assert (read_file [] @@ open_in "data2.txt" = [])
(**/**)

(**/**)
let run_all_tests () =
  test_new_record();
  test_parse();
  test_read_file();
  test_records_insert();
  test_sort_records()
(**/**)

(** main program entry from cli *)
let () =
  if Array.length Sys.argv = 1 then
    Printf.printf "%s: expects 1 file input.\nUsage: \"%s <filename>\"\n" Sys.argv.(0) Sys.argv.(0)
  else
    let ic = open_in Sys.argv.(1) in
    print_records @@ sort_records @@ read_file [] ic;;
