(** Runs the browser with its current state *)

(** [state] defines the state that the web browser is currently in *)
type state

(** [skel state] starts the web browser gui in the initial [state] 
    and updates the state and gui whenever there is a key press or click. *)
val skel : state -> unit