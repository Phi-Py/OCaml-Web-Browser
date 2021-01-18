open Graphics
open Parse
open Webgrab

type page = { 
  tags  : (string * string * string) list; 
  lines : int 
}


(** type [hyperlinks] represents a list of hyperlinks. Each element in the 
    list has a left, bottom coordinate, the hyperlink, and the displayed text *)
type hyperlinks = ((int * int) * string * string) list


(**[onscreen_chars] is the stored text to render to the browser*)
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

(**[empty_chars] is the value of type [onscreen_chars] with no chars present*)
let empty_chars = {
  search     = "";
  navbar     = "";
  navtextbar = "";
  bkgdbar    = "";
  headbar    = "";
  subheadbar = "";
  listbar    = "";
  textbar    = "";
}


(** [open_window] opens a new graphics window *)
let open_window s = 
  open_graph " 640x480";
  set_window_title "WebCaml Browser v.2.0"


(** [render_text text style] renders all text in [text] with [style]*)
let render_text text (style : Style.style) = 
  set_color Graphics.black;
  moveto 420 457;
  draw_string text.search;
  moveto 225 308;
  draw_string text.navbar;
  moveto 225 274;
  draw_string text.navtextbar;
  moveto 225 239;
  draw_string text.bkgdbar;
  moveto 225 204;
  draw_string text.headbar;
  moveto 225 169;
  draw_string text.subheadbar;
  moveto 225 134;
  draw_string text.listbar;
  moveto 225 99;
  draw_string text.textbar


let type_html str (style : Style.style) =
  set_color style.textcolor;
  moveto 420 457;
  draw_string str


let search_bar () =
  set_color white;
  fill_rect 415 455 180 18

(**[refresh style] renders the "->" refresh symbol with color 
   scheme [style]*)
let refresh (style : Style.style) = 
  set_color style.navtextcolor;
  moveto 400 457;
  draw_string "->"


(**[add_tab_logo style] renders the "New Tab" button on the right
   of the screen*)
let add_tab_logo (style : Style.style) = 
  set_color style.navcolor;
  moveto 588 437;
  draw_string "New Tab"


(**[tab_bar style tabs] renders the tab bar consisting of [tabs] 
   with color scheme [style] under the nav bar*)
let tab_bar (style : Style.style) tabs =
  set_color style.navtextcolor;
  fill_rect 0 438 640 13;
  match List.length tabs < 6 with 
  | true -> 
    add_tab_logo style
  | _ -> ()


(**[handle_tab_text style current counter (x,y) rendered_tab x'] 
   renders the tab [rendered_tab], truncating its constituent string
   if necessary*)
let handle_tab_text (style : Style.style) 
    current_tab num_tabs (x,y) rendered_tab x'= 
  if current_tab = num_tabs then begin
    set_color style.listcolor; 
    fill_rect (x+2) y (x' + 15 - x - 2) 13 end
  else ();
  set_color style.navcolor;
  moveto (x + 10) (y);
  let rendered_tab_text, rendered_tab_index = rendered_tab in
  let label = 
    if fst (text_size rendered_tab_text) > 40 
    && (String.length rendered_tab_text > 9)
    then (String.sub rendered_tab_text 0 8) ^ "..."
    else rendered_tab_text
  in draw_string (label)


let draw_tabs (style : Style.style) tabs current =
  let rec helper (x,y) counter = function
    | [] -> ()
    | h :: t -> 
      set_color style.navcolor;
      moveto x y;
      fill_rect x y 1 13;
      let x' = x + 10 + 30 + 40 in
      handle_tab_text style current counter (x,y) h x';
      moveto x' (y-1);
      draw_string "X";
      fill_rect (x' + 15) y 1 13;
      helper ((x' + 15), y) (counter + 1) t
  in helper (10,438) 1 tabs


let clear_bar_logo (style : Style.style) =
  search_bar();
  set_color style.navtextcolor;
  moveto 600 457;
  draw_string "X"

(**[add_bookmark style] renders the "Add" button to add bookmarks
   at the right side of the search bar with color scheme [style]*)
let add_bookmark (style : Style.style) =
  moveto 615 457;
  set_color style.navtextcolor;
  draw_string "Add"

(**[add_scroll style] renders the forward and backward page buttons
   at the bottom right with color scheme [style]*)
let add_scroll (style : Style.style) =
  moveto 5 15;
  set_color style.navtextcolor;
  draw_string "<";
  moveto 632 15;
  draw_string ">"

(**[format str] returns the size of [str] rendered*)
let format str = 
  let (x, y) as size = text_size str in
  size

(**[nav_elts (x,y) elts] renders [elts] horizontally beginning
   at [(x,y)]*)
let rec nav_elts (x,y) = function
  | [] -> ()
  | h :: t -> moveto x y; 
    draw_string h; 
    let size = format h in
    nav_elts (x+fst size+50,y) t


(** [nav] is the default navigation bar of the browser *)
let nav (style : Style.style) (tabs : ('a * int) list) current = 
  set_color style.navcolor;
  fill_rect 0 450 640 30;
  set_color style.backgroundcolor;
  fill_rect 0 0 640 450;
  set_color style.navtextcolor;
  moveto 10 465;
  (* set_font "-*-Helvetica-medium-r-normal--12-*-*-*-*-*-iso8859-1"; *)
  draw_string "WebCaml Browser";
  moveto 10 453;
  let elts = ["Home"; "Bookmarks"; "History"; "Style"] in
  nav_elts (10, 453) elts; 
  search_bar ();
  refresh style;
  clear_bar_logo style;
  tab_bar style tabs;
  draw_tabs style tabs current;
  add_bookmark style

(**[tag_color name style] is the color of tag type[name] 
   in the color scheme [style]*)
let tag_color name (style : Style.style) =
  match name with
  | "title" -> style.headercolor
  | "h1" -> style.headercolor
  | "h2" -> style.subheadcolor
  | "h3" -> style.subheadcolor
  | "ol" -> style.listcolor
  | "ul" -> style.listcolor
  | "a" -> style.navcolor (*hyperlink*)
  | _ -> style.textcolor


(* Functions to create a list of pages of the parsed html file *)
let tag_newline name =
  match name with
  | "title" -> 0
  | "h1" -> 0
  | "h2" -> 0
  | "h3" -> 0
  | "ol" -> 0
  | "ul" -> 0
  | "a" -> 0 
  | _ -> 0


(**[last_page pages] is the last page in [pages] and an empty page 
   if [List.length page = 0]. Equivalent to a safe [List.hd]*)
let last_page (pages : page list) =
  match List.rev pages with 
  | [] -> {tags = []; lines = 0}
  | h :: t -> h


(**[before_last_page pages] is every page in [pages] save for the last.
   Returns the empty list if there exists only one page in [pages]*)
let before_last_page (pages : page list) =
  let length = List.length pages in 
  let rec helper acc_lst counter = function
    | h :: t -> if length != counter then helper (acc_lst @ [h]) (counter+1) t
      else acc_lst
    | [] -> []
  in helper [] 1 pages


(* [wrap_tag] creates a tag list by duplicating [tag] if its contents do not 
   fit on a single line given by the length of [bound]. *)
let wrap_tag (tag : string * string * string) char_bound =
  let rec wrap_aux (acc : (string * string * string) list) tag = 
    match tag with 
    | name, contents, link as tag -> 
      begin
        let length = String.length contents in
        if length = 0 then acc
        else if length > char_bound then
          wrap_aux 
            ((name, (String.sub contents 0 char_bound), link) :: acc) 
            (name, String.sub contents char_bound (length-char_bound), link)
        else tag :: acc
      end
  in List.rev(wrap_aux [] tag)


(**[split_contents wrapped_tag pages] splits [wrapped_tag] into
   mostly equal sized pages to properly display on the GUI without overflow*)
let split_contents 
    (wrapped_tag : (string * string * string) list)
    (pages : page list) : page list =
  let rec helper (pages_acc : page list) wrapped_tag =
    match wrapped_tag with 
    | [] -> pages_acc
    | (name, contents, link) as h :: t -> begin
        let last_page = last_page pages_acc in
        let add_tag_lines = last_page.lines + 1 + (tag_newline name) in
        if add_tag_lines < 28 then (* 28 line limit *)
          helper ((before_last_page pages_acc) @ 
                  [{tags = last_page.tags @ [h]; lines = add_tag_lines}]) t
        else 
          let new_page = {tags = [h]; lines = 1 + (tag_newline name)} in 
          helper (pages_acc @ [new_page]) t 
      end
  in helper pages wrapped_tag


(**[add_tag_to_pages pages tag] appends a wrapped version of [tag]
   to the pages list [pages]*)
let add_tag_to_pages
    (pages : page list) (tag : (string * string * string)) : page list =
  let wrapped_tag = wrap_tag tag 95 in (* arbitrary 50 character limit *)
  split_contents wrapped_tag pages


(**[make_pages tags] is the page list formed from the tags in [tags]
   after wrapping and reformatting to fit the GUI screen*)
let make_pages (tags : (string * string * string) list) : page list =
  let rec helper (tags : (string * string * string) list)
      (pages_acc : page list) = 
    match tags with
    | [] -> pages_acc
    | h :: t -> helper t (add_tag_to_pages pages_acc h) 
  in helper tags []

(**[print_tag style (x,y) tags links_acc] is the list of hyperlinks generated 
   by rendering [tag] at the coordinates [(x,y)] with color scheme [style]*)
let rec print_tag style ((x,y) : int * int) 
    (tag : (string * string * string) list) 
    links_acc : hyperlinks =
  match tag with
  | [] -> links_acc
  | (name, contents, link) :: t ->
    if name = "a" then begin
      moveto x y; 
      set_color (tag_color name style); 
      draw_string contents; 
      print_tag style (x,y-15) t (links_acc @ [(x,y),link,contents])
    end
    else begin
      moveto x y; 
      set_color (tag_color name style); 
      draw_string contents; 
      print_tag style (x,y-15) t links_acc
    end


let page html scroll style (tabs : ('a * int) list) current = 
  nav style tabs current;
  add_scroll style;
  set_color yellow;
  let lst = to_assoc html in 
  let pages = make_pages lst in
  let rec print_page (x,y) counter scroll links_acc = function
    | [] -> []
    | h :: t -> 
      if counter = scroll then 
        print_tag style (x,y) (h.tags) links_acc
      else print_page (x,y) (counter+1) scroll links_acc t
  in 
  let links = print_page (10, 420) 0 scroll [] pages in 
  List.length pages, links


let home (style : Style.style) tabs current =
  nav style tabs current;
  set_color style.headercolor;
  moveto 315 400;
  draw_string "Home";
  set_color style.subheadcolor;
  moveto 100 370;
  draw_string "Quick Guide";
  set_color style.textcolor;
  moveto 100 340;
  draw_string 
    "Seach: type a website address in the search bar and press [return]";
  moveto 100 325;
  draw_string 
    "Bookmark: press 'Add' in the top right of the window to add a bookmark";
  moveto 100 310;
  draw_string 
    "Scroll: click the arrows on the bottom of a website to change the page";
  moveto 100 295;
  draw_string 
    "Refresh: click the arrow next to the search bar to refresh the page";
  moveto 100 280;
  draw_string 
    "Style: press 'Style' on the right of the navigation bar to change browser colors";
  moveto 100 265;
  draw_string "Quit: press [esc]"


(**[print_lst style (x,y) n lst] renders the elements of [lst]
   on the GUI with [n] pixels between them. The first element is 
   rendered at [(x,y)]*)
let rec print_lst (style : Style.style) (x,y) n = function
  | [] -> ()
  | h :: t -> moveto x y; set_color (rgb 100 200 250); draw_string h;
    print_lst style (x, y-n) n t


(**Similar to [print_lst] but in addition renders [x] symbols alongside
   the rendered elements in [lst]*)
let rec print_lst_del (style : Style.style) (x,y) n = function
  | [] -> ()
  | h :: t -> moveto x y; set_color (rgb 100 200 250); 
    if String.length h > 60 then draw_string ((String.sub h 0 60) ^ "...") else
      draw_string h;
    moveto (x-10) y; set_color (rgb 250 0 0); draw_string "x";
    print_lst_del style (x,y-n) n t


let bookmarks (lst : string list list) style tabs current =
  nav style tabs current;
  set_color (rgb 150 232 248);
  moveto 305 400;
  draw_string "Bookmarks";
  let rec helper (x,y) = function
    | [] -> () 
    | h :: t -> print_lst_del style (x,y) 15 h; 
      helper (x,y-15) t;
  in helper (225,380) lst


let history lst style tabs current =
  nav style tabs current;
  set_color (rgb 150 232 248);
  moveto 305 400;
  draw_string "History";
  let rec helper (x,y) = function
    | [] -> () 
    | h :: t -> print_lst_del style (x,y) 15 h; 
      helper (x,y-15) t;
  in helper (225,380) lst


let style (style : Style.style) tabs current =
  nav style tabs current;
  set_color style.headercolor;
  moveto 305 400;
  draw_string "Styles";
  moveto 100 370;
  set_color style.subheadcolor;
  draw_string "Pick a prebuilt style from below";
  print_lst style (100, 340) 15 Style.styles;
  moveto 100 215;
  draw_string "Or ";
  set_color yellow;
  draw_string "define a custom style"


(**[draw_bars (x,y) dist n] draws [n] bars starting at [(x,y)] 
   with [dist] pixels in between each*)
let rec draw_bars (x,y) dist n =
  match n with
  | 0 -> ()
  | i -> 
    set_color white;
    fill_rect x y 180 18;
    draw_string "";
    draw_bars (x, y - dist) dist (n - 1)


(**[custom_style style] draws the custom style page using [style]*)
let custom_style style tabs current =
  nav style tabs current;
  set_color style.headercolor;
  moveto 305 400;
  draw_string "Custom Style Builder";
  moveto 100 370;
  set_color style.subheadcolor;
  draw_string "Custom style instructions:";
  moveto 100 355;
  draw_string "1. Type a color name into each of the fields below";
  moveto 100 340;
  draw_string "2. Press \"Apply\" to apply your custom style";
  let fieldlist = [
    "Navbar color:"; 
    "Navbar text color:"; 
    "Background color:";
    "Header color:";
    "Subheader color:";
    "List color:";
    "Text color:"; 
  ] in
  print_lst style (100, 310) 35 fieldlist;
  draw_bars (220, 307) 35 7;
  moveto 305 50;
  draw_string "APPLY"


(**[render_page page style] renders [page] with [style]*)
let render_text_bars_custom_style () = 
  draw_bars (220, 307) 35 7;