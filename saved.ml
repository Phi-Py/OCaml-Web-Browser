(** Saves bookmarks and history between sessions *)
open Csv

(* rows: bookmarks, history *)
(* string list list *)
type t = string list list

let read_bookmarks s : t = 
  load "saved_files/bookmarks.csv"

let read_history s : t = 
  load "saved_files/history.csv"

let save_bookmark site =
  if site = "" then () else
    save "saved_files/bookmarks.csv" (read_bookmarks() @ [[site]])

let new_bookmarks lst = 
  save "saved_files/bookmarks.csv" lst

let save_to_history site =
  if site = "" then () else
    save "saved_files/history.csv" (read_history() @ [[site]])

let new_history lst = 
  save "saved_files/history.csv" lst

let remove_bookmark site = ()

let clear_history = ()
