(** Saves bookmarks and history between sessions *)

(**type [t] is a representation of the external CSV containing
   the between-session *)
type t = Csv.t


(**[read_bookmarks ()] is the list of bookmarks currently stored
   in the browser*)
val read_bookmarks : 'a -> t 


(**[read_history ()] is the browser history currently stored*)
val read_history : 'a -> t  


(**[save_bookmark site] stores [site] in the external bookmarks storage*)
val save_bookmark : string -> unit


(**[new_bookmarks lst] replaces the current set of bookmarks
   with the data in [lst]*)
val new_bookmarks : t -> unit


(**[save_to_history site] saves [site] to the external history storage*)
val save_to_history : string -> unit


(**[new_history lst] replaces the current set of history
   with the data in [lst]*)
val new_history : t -> unit