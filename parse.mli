(** Parses an html file *)

(** [t] represents an html tag *)
type t

(** [read_file file] is the contents of [file]as a string *)
val read_file: string -> string


(**[lex_and_parse] lexes and parses the input html*)
val lex_and_parse : string -> t

(** [to_assoc file] converts a file into an assoc list that contains 
    elements in the form (id, text), where id is the type of html tag and
    and text is the text inside the html tag.  *)
val to_assoc : string -> (string * string * string) list 

(* functions for unit testing *)

(**[flatten_tags tags] is the flattened representation of 
   type t used for testing*)
val flatten_tags : t -> string

