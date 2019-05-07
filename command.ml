type object_phrase = string list

type command = 
  | Evolve of object_phrase
  | Score
  | Quit
  | Next
  | Upgrades
  | Forward of object_phrase
  | Commands
  | Get of object_phrase 

exception Empty

exception Malformed

let parse str : command =
  if String.trim str = "" then raise Empty else
    let split_str_list = (String.split_on_char ' ' str) in 

    match split_str_list with 
    | h::t when h = "evolve" -> Evolve t
    | h::_ when h = "upgrades" -> Upgrades
    | h::_ when h = "score" -> Score
    | h::_ when h = "quit" ->  Quit
    | h::_ when h = "next" || h = "n" -> Next
    | h::t when h = "forward" || h = "f" -> Forward t
    | h::_ when h = "commands" -> Commands
    | h::t when h = "get" -> Get t
    | _ -> raise Malformed