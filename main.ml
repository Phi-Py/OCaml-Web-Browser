open Parse
open Gui
open Graphics
open Webgrab
open Saved

type env = 
  | Home
  | Bookmarks
  | History
  | Style
  | CustomStyle
  | Website of string

type type_env =
  | Search
  | NavC
  | NavTC
  | BkgdC
  | HeaderC
  | SubhedC
  | ListC
  | TextC

type state = {
  maxx         : int; 
  maxy         : int; 
  mutable pastchars    : string; 
  text         : Gui.onscreen_chars;
  mutable bookmarks    : string list;
  mutable history      : string list;
  hyperlinks   : Gui.hyperlinks;
  tabs         : (env * int) list;
  current_tab  : int;
  page_count   : int;
  total_pages  : int;
  current_page : env;
  type_env     : type_env;
  scale        : int;
  bc           : color;
  fc           : color; 
  pc           : color;
  style        : Style.style;
}

(* [End] is raised to close the window *)
exception End

(* [match_env] is a string representation of the environment *)
let match_env = function
  | Home -> "Home"
  | Bookmarks -> "Bookmarks"
  | History -> "History"
  | Style -> "Style"
  | CustomStyle -> "Custom Style"
  | Website site -> site

(* [string_of_env_tabs] is a string represntation of the label for each tab 
   in [tabs] *)
let string_of_env_tabs tabs = 
  let rec helper tabs_acc = function
    | [] -> tabs_acc
    | (env, int) :: t -> helper (tabs_acc @ [(match_env env, int)]) t
  in helper [] tabs

(**[t_init is the state after initializing the window and displaying the
    homepage] *)
let t_init st = 
  open_window ();
  home st.style (string_of_env_tabs st.tabs) st.current_tab;
  search_bar ();
  {st with current_page = Home}

(* [t_end] closes the browser window *)
let t_end () = 
  close_graph();
  print_string "Leaving WebCaml... Farewell."; 
  print_newline()

let rec fit_str_from_start str_acc = function
  | true -> if fst (text_size str_acc) < 170 then str_acc 
    else fit_str_from_start 
        (String.sub str_acc 0 (String.length str_acc - 2)) true
  | false -> if fst (text_size str_acc) < 170 then str_acc 
    else fit_str_from_start 
        (String.sub str_acc 1 (String.length str_acc - 1)) false

(* [type_fit_window str] fits the website address [str] in the search bar. *)
let type_fit_window str from_start = 
  let str_size = fst (text_size str) in
  if str_size < 170 then str
  else if not (str_size < 170) && from_start then fit_str_from_start str true
  else fit_str_from_start str false

(* [change_tab s tab] is a list of tabs that replaces the current tab with 
   [tab] *)
let change_tab s tab = 
  let rec helper counter tab_acc  = function
    | [] -> failwith "impossible"
    | h :: t -> if counter = s.current_tab then tab_acc @ [tab] @ t 
      else helper (counter + 1) (tab_acc @ [h]) t
  in helper 1 [] s.tabs

(* [add_history website] saves [website] to history *)
let add_history website = 
  if List.length (read_history []) > 20 then begin
    new_history (List.tl (read_history []));
    save_to_history website 
  end
  else save_to_history website

(* [website (st, scroll) s] renders a website at page [scroll] in state [s]*)
let website ((st, scroll) : state * int) s =
  let total, links = page (webgrab s.pastchars) scroll s.style 
      (string_of_env_tabs st.tabs) st.current_tab in
  type_html (type_fit_window s.pastchars true) s.style;
  add_history s.pastchars;
  let s' = {st with hyperlinks = links; total_pages = total; 
                    tabs = change_tab s (Website s.pastchars,s.current_tab);
                    current_page = Website s.pastchars;
                    history = s.history @ [s.pastchars]}
  in draw_tabs s'.style (string_of_env_tabs s'.tabs) s'.current_tab; s'

let match_tab s (str, int)= 
  let s' = {s with tabs = change_tab s (str, int)} in
  match str with 
  | Home -> nav s'.style (string_of_env_tabs s'.tabs) s'.current_tab; 
    home s.style (string_of_env_tabs s'.tabs) s.current_tab; s'
  | Bookmarks -> nav s'.style (string_of_env_tabs s'.tabs) s'.current_tab; 
    bookmarks (read_bookmarks()) s.style 
      (string_of_env_tabs s'.tabs) s.current_tab; s'
  | History -> nav s'.style (string_of_env_tabs s'.tabs) s'.current_tab; 
    history (read_history()) s.style 
      (string_of_env_tabs s'.tabs) s.current_tab; s'
  | Style -> nav s'.style (string_of_env_tabs s'.tabs) s'.current_tab; 
    style s.style (string_of_env_tabs s'.tabs) s.current_tab; s'
  | Website site -> nav s'.style (string_of_env_tabs s'.tabs) s'.current_tab; 
    ignore (website (s, 0) s); s'
  | _ -> s

(* [get_tab tab_num tabs] is the tab at position [tab_num] in [tabs] *)
let get_tab tab_num (tabs : (env * int) list) = 
  let rec helper counter = function
    | [] -> failwith "impossible"
    | h :: t -> if counter = tab_num then h else helper (counter + 1)  t
  in helper 1 tabs

(* [display_tab s] is the state [s] with the current tab displayed *)
let display_tab s = 
  let tab = (s.current_page, snd (get_tab s.current_tab s.tabs)) in 
  match_tab s tab

let display_home_page st = 
  let s' = {st with current_page = Home} in 
  let s'' = display_tab s' in
  home s''.style 
    (string_of_env_tabs s''.tabs) s''.current_tab; s''

let display_bookmarks st =
  let s' = {st with current_page = Bookmarks} in 
  let s'' = display_tab s' in
  bookmarks (read_bookmarks()) s''.style 
    (string_of_env_tabs s''.tabs) s''.current_tab; s''

let display_history st =
  let s' = {st with current_page = History} in 
  let s'' = display_tab s' in
  history (read_history()) s''.style 
    (string_of_env_tabs s''.tabs) s''.current_tab; s''

let display_style st =
  let s' = {st with current_page = Style} in 
  let s'' = display_tab s' in
  style s''.style 
    (string_of_env_tabs s''.tabs) s''.current_tab; s''

(* [nav_elt st (x,y)] is [st] after an element in the navigation bar at [(x,y)] 
   is clicked *)
let nav_elt st (x,y) = 
  let y_bound = y > 455 && y < 465 in
  match x with 
  | x when x > 10 && x < 40 && y_bound ->
    display_home_page st
  | x when x > 82 && x < 140 && y_bound -> 
    display_bookmarks st
  | x when x > 188 && x < 235 && y_bound -> 
    display_history st
  | x when x > 280 && x < 315 && y_bound -> 
    display_style st
  | _ -> st

(* [style_elt st (x,y)] is st with the style corresponding to
   the (x,y) location on the style page*)  
let style_elt st (x,y) = 
  match x, y with
  | x, y when y <= 350 && y > 336 && x > 100 && x < 140 -> 
    {st with style = Style.default}
  | x, y when y <= 335 && y > 326 && x > 100  && x < 125 ->   
    {st with style = Style.dark}
  | x, y when y <= 320 && y > 310 && x > 100  && x < 135 -> 
    {st with style = Style.desert}
  | x, y when y <= 315 && y > 299 && x > 100  && x < 140 -> 
    {st with style = Style.hacker}
  | x, y when y <= 293 && y > 283 && x > 100  && x < 125 -> 
    {st with style = Style.light}
  | x, y when y <= 277 && y > 267 && x > 100  && x < 154 -> 
    {st with style = Style.minimalist}
  | x, y when y <= 262 && y > 250 && x > 100  && x < 160 -> 
    {st with style = Style.powershell}
  | x, y when y <= 220 && y > 205 && x <= 305 && x > 100 ->    
    (custom_style st.style (string_of_env_tabs st.tabs) st.current_tab;
     {st with current_page = CustomStyle})
  | x, y -> st

(* [change_page st counter max_page] is [st] with the current incremented by 
   [counter]. *)
let change_page st counter max_page =
  let change = st.page_count + counter in
  let scroll = if change < 0 then 0 else 
    if change >= max_page then 0 else counter in
  let new_page_count = st.page_count + scroll in 
  let st' = {st with page_count = new_page_count} in
  st', st'.page_count

let get_core_website url  =
  let httpregex = Str.regexp "https://" in
  let pruned_url =
    if Str.string_match httpregex url 0
    then Str.replace_matched "" url
    else url
  in
  let pos = String.index_opt pruned_url '/' in
  match pos with
  | Some i -> String.sub pruned_url 0 i
  | None -> pruned_url

(*[handle_root_link st link] is the proper address from the root of
   the website *)
let handle_root_link st link = 
  match st.current_page with
  | Website url -> get_core_website url ^ link
  | env -> ""

(*[handle_leaf_link st link] is the proper address of the leaf link
   (a link accessing a page in the current directory)*)
let handle_leaf_link st link = 
  match st.current_page with
  | Website url -> url ^ "/" ^ link
  | env -> ""

(*[get_link_address] st link is the true address of the hyperlink,
   accounting for internal website links*)
let get_link_address st link = 
  let test_grab = webgrab link in
  if test_grab <> "<p>Oh no! Cannot not find the website.</p>"
  then link
  else if link.[0] = '/'
  then handle_root_link st link 
  else handle_leaf_link st link

(**[visit_hyperlink ((x,y), link, contents) s] visits hyperlink [link]*)
let visit_hyperlink ((x,y), link, contents) s =
  let link' = get_link_address s link in
  let st' = {s with pastchars = link'; 
                    text = {s.text with search = type_fit_window link' true}} in
  save_to_history s.pastchars; (* HISTORY *)
  website (st', 0) st'

let link_elt (x,y) s =
  let rec helper links = 
    match links with 
    | [] -> false, ((x,y), "toss", "away")
    | ((lx,ly), link, contents) as h :: t -> begin
        let linkdim = text_size contents in
        let dim_x = fst linkdim + lx in
        let dim_y = snd linkdim + ly in
        match x, y with 
        | x, y when x > lx && x < dim_x && y > ly && y < dim_y -> true, h
        | _ -> helper t
      end
  in helper s.hyperlinks

(**[handle_style_click st (x,y)] is [st] after processing a user click at
   [(x,y)]*)
let handle_style_click st (x,y) = 
  let st' = style_elt st (x,y) in
  if st' <> st && st'.current_page = CustomStyle
  then (custom_style st'.style 
          (string_of_env_tabs st'.tabs) st'.current_tab; st')
  else if st' <> st 
  then (style st'.style 
          (string_of_env_tabs st'.tabs) st'.current_tab; st')
  else 
    let st' = nav_elt st (x,y) in
    if st'.current_page <> st.current_page
    then {st' with text = Gui.empty_chars} 
    else st

(**[package_color_list st] is the extracted text from the custom style color
   fields*)
let package_color_list st = 
  let fields = st.text in 
  [fields.navbar; 
   fields.navtextbar; 
   fields.bkgdbar;
   fields.headbar;
   fields.subheadbar;
   fields.listbar;     
   fields.textbar]

let custom_style_elt st (x,y) =
  let x_bound = x > 220 && x < 400 in
  match x, y with
  | x, y when x <= 595 && x > 415 && y <= 473 && y > 455 -> 
    {st with type_env = Search}
  | x, y when y <= 325 && y > 307 && x_bound -> 
    {st with type_env = NavC}
  | x, y when y <= 290 && y > 272 && x_bound-> 
    {st with type_env = NavTC}
  | x, y when y <= 255 && y > 237 && x_bound -> 
    {st with type_env = BkgdC}
  | x, y when y <= 220 && y > 202 && x_bound -> 
    {st with type_env = HeaderC}
  | x, y when y <= 185 && y > 167 && x_bound -> 
    {st with type_env = SubhedC}
  | x, y when y <= 150 && y > 132 && x_bound -> 
    {st with type_env = ListC}
  | x, y when y <= 115 && y > 97 && x_bound -> 
    {st with type_env = TextC}
  | x, y  when y < 60 && y > 45 && x >= 305 && x < 350 ->
    let field_vals = package_color_list st in
    let style' = Style.build_style field_vals in
    style style' (string_of_env_tabs st.tabs) st.current_tab;
    {st with current_page = Style; style = style'; 
             text = Gui.empty_chars; type_env = Search;} 
  | x, y -> st

(**[handle_custom_style_click st (x,y)] is st after processing the user 
   click at [(x,y)]*)
let handle_custom_style_click st (x,y) = 
  let st' = custom_style_elt st (x,y) in
  if  st' <> st then st'
  else let st'' = nav_elt st' (x,y) in
    if st''.current_page <> st'.current_page 
    then {st'' with text = Gui.empty_chars}
    else st''

let rec del_pos lst pos acclst acc = 
  match lst with
  | [] -> acclst
  | h :: t -> if pos = acc then del_pos t pos acclst (acc+1) 
    else del_pos t pos (acclst@[h]) (acc+1)

let bookmark_hist_del lst y =
  let ycheck = 390 - y in 
  let ypos = ycheck / 15 in 
  del_pos lst ypos [] 0

let rec to_lstlst lst acc = match lst with
  | [] -> acc
  | h :: t -> to_lstlst t (acc @ [[h]])

let rec to_lst lst acc = match lst with
  | [] -> acc
  | h :: t -> match h with 
    | [] -> failwith "not possible"
    | a :: b -> to_lst t (acc @ [a])

let linkcheck y =
  let check = 390 - y in 
  let position = check / 15 in 
  position

(* [add_tab s] adds a new tab to the list of tabs in state [s] *)
let add_tab s : state = 
  if (List.length s.tabs) < 6
  then {s with pastchars = ""; text= {s.text with search = ""}; 
               tabs = (s.tabs @ [(Home, (s.current_tab + 1))]); 
               current_tab = s.current_tab + 1; current_page = Home}
  else s 

(* remove_tab counter tab_num tab_acc] is a list of tabs with the tab at 
   [counter] removed in the list of tabs *)
let rec remove_tab counter tab_num tab_acc = function
  | [] -> tab_acc
  | h :: t -> if counter = tab_num then tab_acc @ t 
    else remove_tab (counter+1) tab_num (tab_acc@[h]) t

(* *[delete_tabs x y tabs] is [tabs] with the deleted tab at [x] and [y] 
   removed *)
let delete_tab (x : int) (y : int) tabs = 
  let y_bound = y > 438 && y < 450 in
  match x with 
  | x when x > 91 && x < 101 && y_bound -> 
    if List.length tabs > 1 then remove_tab 1 1 [] tabs, true 
    else if List.length tabs = 0 then [(Home, 1)], true
    else tabs, false
  | x when x > 185 && x < 195 && y_bound -> 
    if List.length tabs >= 2 then remove_tab 1 2 [] tabs, true else tabs, false
  | x when x > 280 && x < 290 && y_bound -> 
    if List.length tabs >= 3 then remove_tab 1 3 [] tabs, true else tabs, false
  | x when x > 375 && x < 385 && y_bound -> 
    if List.length tabs >= 4 then remove_tab 1 4 [] tabs, true else tabs, false
  | x when x > 470 && x < 480 && y_bound -> 
    if List.length tabs >= 5 then remove_tab 1 5 [] tabs, true else tabs, false
  | x when x > 565 && x < 575 && y_bound -> 
    if List.length tabs >= 6 then remove_tab 1 6 [] tabs, true else tabs, false
  | _ -> tabs, false

let handle_nav_text = function
  | "Home" | "Bookmarks" | "History" | "Style" | "Custom Style" -> ""
  | s -> s

let tab_grab int s tabs = 
  let tab = get_tab int tabs in
  let site = handle_nav_text (fst (List.hd (string_of_env_tabs [tab]))) in
  {s with pastchars = site; 
          text= {s.text with search = type_fit_window site true}; 
          current_tab = int; current_page = fst tab}, true

(* [click_tab s x y tabs] creates a new state when a tab is clicked *)
let click_tab s (x : int) y (tabs : (env * int) list) = 
  let y_bound = y > 436 && y < 451 in
  match x with 
  | x when x > 11 && x < 105 && y_bound && List.length tabs >= 1 -> 
    tab_grab 1 s tabs
  | x when x > 108 && x < 200 && y_bound && List.length tabs >= 2 -> 
    tab_grab 2 s tabs
  | x when x > 204 && x < 294 && y_bound && List.length tabs >= 3 -> 
    tab_grab 3 s tabs
  | x when x > 298 && x < 388 && y_bound && List.length tabs >= 4 -> 
    tab_grab 4 s tabs
  | x when x > 392 && x < 484 && y_bound && List.length tabs >= 5 -> 
    tab_grab 5 s tabs
  | x when x > 488 && x < 580 && y_bound && List.length tabs <= 6 -> 
    tab_grab 6 s tabs
  | _ -> s, false

let new_current_tab s = 
  if List.length s.tabs = 1 then 1
  else if s.current_tab = List.length s.tabs then s.current_tab - 1
  else s.current_tab

let tab_search_state tabs current s = 
  let current_tab = get_tab current tabs in
  let site = fst (List.hd (string_of_env_tabs [current_tab])) in
  let s' = {s with pastchars = site; text= {s.text with search = site}} in s'

(* [new_bookmark website] creates a new bookmark of [swebsite] *)
let new_bookmark website = 
  let current_bookmarks = read_bookmarks [] in 
  if List.exists (fun x -> x = [website]) current_bookmarks then () else
  if List.length current_bookmarks > 20 then begin
    new_bookmarks (List.tl current_bookmarks);
    save_bookmark website end
  else save_bookmark website

(* [tab_delete_nav st deleted] changes [st] after a tab is deleted *)
let tab_delete_nav st deleted = 
  if (List.length (fst deleted) = 1) 
  && (fst (List.hd (fst deleted)) = Home) 
  then let s' = 
         {st with tabs = fst deleted; current_tab = new_current_tab st; 
                  current_page = Home}
    in nav s'.style (string_of_env_tabs s'.tabs) s'.current_tab; 
    display_tab s'
  else if (List.length (fst deleted) = 0) 
       && (fst (List.hd (fst deleted)) = Home) 
  then let s' = {st with pastchars = ""; tabs = fst deleted; 
                         current_tab = new_current_tab st; 
                         current_page = Home; text= {st.text with search = "";}}
    in nav s'.style (string_of_env_tabs s'.tabs) s'.current_tab; 
    display_tab s'
  else let s' = 
         {st with tabs = fst deleted; current_tab = new_current_tab st} 
    in nav s'.style (string_of_env_tabs s'.tabs) s'.current_tab; 
    display_tab s'

(* [tab_nav st x y] is [st] with deletion or addition of tabs *)
let tab_nav st x y = 
  let deleted = delete_tab x y st.tabs in
  let clicked = click_tab st x y st.tabs in
  if snd deleted then
    tab_delete_nav st deleted
  else if snd clicked then
    let s' = fst clicked in nav s'.style 
      (string_of_env_tabs s'.tabs) s'.current_tab;
    display_tab s'
  else st

let delete_bookmark st y = 
  st.bookmarks <- to_lst (read_bookmarks []) [];
  new_bookmarks (to_lstlst (bookmark_hist_del st.bookmarks y) []);
  bookmarks (read_bookmarks ()) st.style 
    (string_of_env_tabs st.tabs) st.current_tab; st

let delete_history st y = 
  st.history <- to_lst (read_history []) [];
  new_history (to_lstlst (bookmark_hist_del st.history y) []);
  history (read_history ()) st.style 
    (string_of_env_tabs st.tabs) st.current_tab; st

let click_bookmark st y = 
  st.bookmarks <- to_lst (read_bookmarks []) [];
  let lstpos = linkcheck y in 
  let url = List.nth st.bookmarks lstpos in 
  st.pastchars <- url;
  let s' = {st with text = {st.text with search = type_fit_window url true}} 
  in website (s', 0) s'

let click_history st y =
  st.history <- to_lst (read_history []) [];
  let lstpos = linkcheck y in 
  let url = List.nth st.history lstpos in 
  st.pastchars <- url;
  let s' = {st with text = {st.text with search = type_fit_window url true}} 
  in website (s', 0) s'

let handle_t_mouse_otherwise st x y = 
  let link_coord = link_elt (x,y) st in
  let st' = nav_elt st (x,y) in
  if st' <> st
  then {st' with pastchars = ""; text = {st'.text with search = ""}} 
  else if fst link_coord then begin visit_hyperlink (snd link_coord) st end
  else st

let t_mouse_add_tab st = begin
  let s' = add_tab st in nav s'.style 
    (string_of_env_tabs s'.tabs) s'.current_tab; 
  display_tab s' end

(**[t_mouse st x y] is the state at t' from a mouse press at state t
  * requires 0 < x < 600, 0 < y < 480*)
let t_mouse st x y = 
  match x, y with 
  | x, y when x > 599 && x < 609 && y > 458 && y < 470 -> 
    clear_bar_logo st.style; 
    {st with pastchars = ""; text= {st.text with search = ""}}
  | x, y when x > 615 && x < 640 && y > 458 && y < 470 -> 
    new_bookmark st.pastchars; st
  | x, y when (x > 4 && x < 11 && y > 16 && y < 25) -> 
    website (change_page st (-1) st.total_pages) st
  | x, y when (x > 633 && x < 639 && y > 16 && y < 25) -> 
    website (change_page st (1) st.total_pages) st
  | x, y when (x > 589 && x < 639 && y > 437 && y < 448) -> 
    t_mouse_add_tab st
  | w, y when (x > 395 && x < 415 && y > 455 && y < 470) ->
    if st.pastchars = "" then st else website (st, 0) st
  | x, y when (y > 437 && y < 448) -> tab_nav st x y
  | x, y when st.current_page = Style -> 
    handle_style_click st (x, y)
  | x, y when st.current_page = CustomStyle ->
    handle_custom_style_click st (x, y)
  | x, y when (x > 200 && x < 225 && y < 450 && st.current_page = Bookmarks) ->
    delete_bookmark st y
  | x, y when (x > 200 && x < 225 && y < 450 && st.current_page = History) ->
    delete_history st y
  | x, y when (x > 225 && x < 500 && y < 450 && st.current_page = Bookmarks) ->
    click_bookmark st y
  | x, y when (x > 225 && x < 500 && y < 450 && st.current_page = History) ->
    click_history st y
  | _ -> handle_t_mouse_otherwise st x y

(**[t_except] nullifies any non-End exception*)
let t_except (ex : exn) = ()

(**[delete_from_string str] deletes the last char in str*)
let delete_from_string str =
  let pastchars_length = String.length str in
  let str' =
    if pastchars_length > 0 then
      String.sub str 0 (pastchars_length - 1) 
    else 
      ""
  in
  str'

(**[delete_from_search st] deletes the last char in [st.text.search]
   and [st.pastchars]*)
let delete_from_search st =
  let new_pastchars = delete_from_string st.pastchars in
  let new_text = type_fit_window new_pastchars false in
  {st with text = {st.text with search = new_text}; 
           pastchars = new_pastchars}


(**[delete_from_navbar st] deletes the last char in [st.text.navbar]*)
let delete_from_navbar st =
  let new_text = delete_from_string st.text.navbar in
  {st with text = {st.text with navbar = new_text}}


(**[delete_from_navtextbar st] deletes the last char in [st.text.navtextbar]*)
let delete_from_navtextbar st =
  let new_text = delete_from_string st.text.navtextbar in
  {st with text = {st.text with navtextbar = new_text}}


(**[delete_from_bkgd st] deletes the last char in [st.text.bkgdbar]*)
let delete_from_bkgdbar st =
  let new_text = delete_from_string st.text.bkgdbar in
  {st with text = {st.text with bkgdbar = new_text}}


(**[delete_from_headbar st] deletes the last char in [st.text.headbar]*)
let delete_from_headbar st =
  let new_text = delete_from_string st.text.headbar in
  {st with text = {st.text with headbar = new_text}}


(**[delete_from_subheadbar st] deletes the last char in [st.text.subheadbar]*)
let delete_from_subheadbar st =
  let new_text = delete_from_string st.text.subheadbar in
  {st with text = {st.text with subheadbar = new_text}}


(**[delete_from_listbar st] deletes the last char in [st.text.listbar]*)
let delete_from_listbar st =
  let new_text = delete_from_string st.text.listbar in
  {st with text = {st.text with listbar = new_text}}


(**[delete_from_textbar st] deletes the last char in [st.text.listbar]*)
let delete_from_textbar st =
  let new_text = delete_from_string st.text.textbar in
  {st with text = {st.text with textbar = new_text}}


(**[delete_last st] is st with the last char in pastchars removed*)
let delete_last (st : state) =
  let st' = 
    match st.type_env with
    | Search -> delete_from_search st
    | NavC -> delete_from_navbar st
    | NavTC -> delete_from_navtextbar st
    | BkgdC -> delete_from_bkgdbar st
    | HeaderC -> delete_from_headbar st
    | SubhedC -> delete_from_subheadbar st
    | ListC -> delete_from_listbar st
    | TextC -> delete_from_textbar st
  in    
  clear_bar_logo st'.style;
  st'

(**[add_to_history state] is [state] with the string in 
   [pastchars] added and [pastchars] reset to the empty string*)
let add_to_history state =
  if List.mem state.pastchars state.history then state
  else let new_history = state.history @ [state.pastchars] in
    {state with history = new_history}

(**[add_char_to_state st str] is [st] with [str] added to the current typing
    environment*)
let add_char_to_state st str = 
  match st.type_env with
  | Search -> 
    let new_pastchars = st.pastchars ^ str in
    let new_text = st.text.search ^ str in
    let truncated = type_fit_window new_text false in
    {st with text = {st.text with search = truncated}; 
             pastchars = new_pastchars}
  | NavC -> 
    let new_text = st.text.navbar ^ str in
    {st with text = {st.text with navbar = new_text}}
  | NavTC -> let new_text = st.text.navtextbar ^ str in
    {st with text = {st.text with navtextbar = new_text}}
  | BkgdC -> let new_text = st.text.bkgdbar ^ str in
    {st with text = {st.text with bkgdbar = new_text}}
  | HeaderC -> let new_text = st.text.headbar ^ str in
    {st with text = {st.text with headbar = new_text}}
  | SubhedC -> let new_text = st.text.subheadbar ^ str in
    {st with text = {st.text with subheadbar = new_text}}
  | ListC -> let new_text = st.text.listbar ^ str in
    {st with text = {st.text with listbar = new_text}}
  | TextC -> let new_text = st.text.textbar ^ str in 
    {st with text = {st.text with textbar = new_text}}

(**[t_key st c] is the universal handler of keypresses*)                                                                        
let t_key st c = 
  match c with 
  | '\b' -> delete_last st
  | '\r' -> 
    if st.type_env = Search 
    then 
      let st' = add_to_history st in 
      website (st', 0) st'
    else st
  | '\027' -> raise End
  | c -> add_char_to_state st (Char.escaped c)

let stel = {
  maxx         = 120; 
  maxy         = 120; 
  pastchars    = ""; 
  text         = Gui.empty_chars;
  bookmarks    = []; 
  history      = []; 
  hyperlinks   = [];
  tabs         = [(Home, 0)];
  current_tab  = 1;
  page_count   = 0; 
  total_pages  = 0;
  current_page = Home;
  type_env     = Search;
  scale        = 4; 
  bc           = Graphics.rgb 130 130 130;
  fc           = Graphics.red; 
  pc           = Graphics.red; 
  style        = Style.default};;

let skel state  = 
  let init_state = t_init state in
  let state = ref init_state in
  try 
    while true do 
      try 
        let status = Graphics.wait_next_event 
            [Graphics.Button_down; Graphics.Key_pressed] in
        let st' = 
          if status.Graphics.keypressed 
          then t_key !state status.Graphics.key
          else if status.Graphics.button 
          then t_mouse !state status.Graphics.mouse_x status.Graphics.mouse_y
          else !state
        in
        if st'.current_page = CustomStyle 
        then render_text_bars_custom_style ()
        else ();
        search_bar ();
        render_text Gui.empty_chars st'.style;
        render_text st'.text st'.style;
        state := st'
      with 
        End -> raise End
      |  e  -> t_except e
    done
  with 
    End  -> t_end ()

let slate () = 
  skel stel 

let () = slate()




