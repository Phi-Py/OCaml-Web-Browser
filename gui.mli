(** Renders all graphical content *)

(** [hyperlinks] represents a list of hyperlinks. Each element in the 
    list has a left, bottom coordinate, the hyperlink, and the displayed text *)
type hyperlinks = ((int * int) * string * string) list

(** [onscreen_chars] is all the user-inputted text in each typing environment*)
type onscreen_chars = {
  search     : string; (** text on the searchbar *)
  navbar     : string; (** text on the navcolor picker bar *)
  navtextbar : string; (** text on the navtextcolor picker bar*)
  bkgdbar    : string; (** text on the backgroundcolor picker bar *)
  headbar    : string; (** text on the headercolor picker bar *)
  subheadbar : string; (** text on the subheadcolor picker bar *)
  listbar    : string; (** text on the listcolor picker bar *)
  textbar    : string; (** text on the textcolor picker bar *)
}

(**[empty_chars] is an empty [onscreen_chars]*)
val empty_chars : onscreen_chars

(** [open_window] renders an empty window *)
val open_window : unit -> unit

(** [render_text chars style] renders all onscreen user-inputted text in 
    [chars] with the [style] color scheme*)
val render_text : onscreen_chars -> Style.style -> unit

(**[type_html str style] renders [str] in the top right search bar
    with [style] color scheme*)
val type_html : string -> Style.style -> unit

(** [search_bar] renders an empty search bar at the top right of the screen*)
val search_bar : unit -> unit 

(** [clear_bar_logo] renders an empty search bar at the top right of the 
    screen *)
val clear_bar_logo : Style.style -> unit

(** [draw_tabs style tabs i] renders the tabs in [tabs] with [style]
    color scheme*)
val draw_tabs : Style.style -> (string * 'a) list -> int -> unit

(** [page html i style tabs i links] renders the webapge from the raw 
    [html], along with the tabs in [tabs] *)
val page : 
  string -> int -> Style.style -> (string * int) list -> int -> int * hyperlinks

(** [home style tabs i] renders the homepage and tabs in [tabs] with 
    the current tab at [i] indicated*)
val home : Style.style -> (string * int) list -> int -> unit

(** [bookmarks bkms style tabs i] renders the bookmarks page with 
    the bookmarks in [bkms] listed; renders [tabs] and bolds current tab
    at [i]*)
val bookmarks : 
  string list list -> Style.style -> (string * int) list -> int -> unit

(** [history hist style tabs i] renders the history page with 
    the history in [bkms] listed; renders [tabs] and bolds current tab
    at [i]*)
val history : 
  string list list -> Style.style -> (string * int) list -> int -> unit

(** [recommended style tabs i] renders the style page with 
    the default styles listed; renders [tabs] and bolds current tab
    at [i]*)
val style : Style.style -> (string * int) list -> int -> unit

(** [custom_style style tabs i] renders the style creator page; 
    renders [tabs] and bolds current tab at [i]*)
val custom_style : Style.style -> (string * int) list -> int -> unit

(** [render_text_bars_custom_style ()] renders the input bars
    on the custom style page*)
val render_text_bars_custom_style : unit -> unit

(** [nav style tabs i] renders the navigation bar on the page with 
    color scheme [style] as well as renders [tabs] and highlights 
    current tab [tabs_i]*)
val nav : Style.style -> (string * int) list -> int -> unit
