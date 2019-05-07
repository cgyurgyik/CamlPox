(** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["evolve"], then the object phrase is 
      [["evolve"]].
    - If the player command is ["     quit   "], then the object phrase is
      again [["quit"]]. 
      An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command =
  | Evolve of object_phrase
  | Score
  | Quit
  | Next
  | Upgrades
  | Forward of object_phrase
  | Commands
  | Get of object_phrase 


(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    quit   "] is [Quit].
    - [parse "score"] is the current score for the player.
      Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
      characters (only ASCII character code 32; not tabs or newlines, etc.).
      Raises: [Empty] if [str] is the empty string or contains only spaces. 
      Raises: [Malformed] if the command is malformed. *)
val parse : string -> command