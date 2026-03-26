(*
org(123,BCIT)
org(345,Hell Holdings\, Ltd)
person(666,name(monty,burns))
org(456,Ocaml \(LLC\))
*)
open Angstrom

type name = Name of string * string
type client = Person of int * name | Org of int * string

let name first last = Name (first, last)
let person id name = Person (id, name)
let org id name = Org (id, name)

let a_char =
  (string "\\(" *> return '(') <|>
  (string "\\)" *> return ')') <|>
  (string "\\," *> return ',') <|>
  (string "\\n" *> return '\n') <|>
  satisfy (fun c -> c <> '(' && c <> ')' && c <> ',' && c <> '\n')

let a_string =
  many1 a_char >>| fun l -> l |> List.to_seq |> String.of_seq


let uint = take_while1 (function | '0'..'9' -> true | _ -> false) >>| int_of_string

let a_name = name <$> string "name(" *> a_string <* char ',' <*> a_string <* char ')'

let a_person = person <$> string "person(" *> uint <* char ',' <*> a_name <* char ')'

let an_org = org <$> string "org(" *> uint <* char ',' <*> a_string <* char ')'

let a_client = a_person <|> an_org
                                       
let parse file =
  let ic = open_in file in
  let content = really_input_string ic (in_channel_length ic) in
  parse_string ~consume:All (many (a_client <* end_of_line)) content

