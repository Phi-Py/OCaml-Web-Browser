open Str
(** AF: transform an HTML file into type t, and then 
    print and display information on a text-based UI 
*)

(* type [tag] represents the name and contents of an html tag. *)
type tag = {
  name: string;
  contents: string;
  hyperlink: string;
}

type t = tag list

type token =
  | Opening of (string * string)
  | Closing of string
  | Word of string
  | Comment
  | Illegal_opening of string
  | Illegal_closing of string
  | Illegal_token


type lexerhelper = {
  token: token;
  lextail: string;
}

type listparsehelper = {
  tag: tag;
  listtail: token list;
}

type tokstack = string Stack.t

(** [print_basic file] prints the contents of the html file. *)
let print_basic file = 
  let in_channel = open_in file in
  try 
    while true do
      let line = input_line in_channel in
      print_endline line
    done
  with End_of_file -> close_in in_channel

let read_file file = 
  let phrases = ref [] in
  let channel = open_in file in
  try
    while true; do
      phrases := input_line channel :: !phrases
    done; !phrases |> String.concat " "
  with End_of_file ->
    close_in channel;
    List.rev !phrases |> String.concat " "

(**[remove_head string] is the body element of [string] 
    preconditions: [string] is well-formed html*)
let remove_head string =
  let regex = Str.regexp "<head>.*</head>" in
  Str.global_replace regex "" string

(**[remove_divs string] is [string] with all <div> tags removed
    preconditions: [string] is well-formed html*)
let remove_divs string = 
  let regex = Str.regexp "<[/]?div[^<>]*>" in
  Str.global_replace regex "" string

(**[explode string] is the char list representation of [string]*)
let to_char_list string =
  let len = String.length string in
  let rec char_aux string i acc = 
    match i with
    | -1 -> acc
    | j -> char_aux string (j - 1) (string.[j]::acc)
  in
  char_aux string (len - 1) [] 

(**[separate_tags clist] is [clist] without special whitespace characters
    and with spaces before '<' and after '>':
    e.g. s1<t1>s2 -> s1 <t1> s2*)
let separate_tags clist = 
  let rec sep_aux clist acc =
    match clist with
    | [] -> List.rev acc
    | '\r'::t
    | '\t'::t
    | '\n'::t -> sep_aux t acc
    | '<'::t -> sep_aux t ("<"::" "::acc)
    | '>'::t -> sep_aux t (" "::">"::acc)
    | h::t -> sep_aux t ((Char.escaped h)::acc)
  in
  sep_aux clist []

(**[preprocess string] is a [string] pipelined through
    the above four functions and joined by ""*)
let preprocess string =
  string
  |> to_char_list
  |> separate_tags
  |> String.concat "" 
  |> remove_head
  |> remove_divs

(**regex to match legal anchor tags *)
let anchor_open_regex = 
  Str.regexp "<a [ &:/\\.=a-zA-Z0-9\" _-]*>"

(**regex to match legal opening tags *)
let legal_opening_regex = 
  Str.regexp "\\(<\\(p\\|h1\\|h[1-6]\\|[ou]l\\|li\\) [ \\.=a-zA-Z0-9\"_-]*>\\)\\|\\(<\\(p\\|h1\\|h[1-6]\\|[ou]l\\|li\\)>\\)"

(**regex to match legal closing tags *)
let legal_closing_regex = 
  Str.regexp "</\\(p\\|h1\\|h[1-6]\\|[ou]l\\|li\\|a\\)>"

(**regex to match non-tag words *)
let word_regex = 
  Str.regexp "[^ \n\r]+"

(**regex to match html comments *)
let comment_regex =
  Str.regexp "<![^<>]*>"

(**regex to match illegal opening tags *)
let illegal_opening_regex =
  Str.regexp "<[a-z]+ ?[^<>]*>"

(**regex to match illegal closing tags *)
let illegal_closing_regex =
  Str.regexp "</[a-z]+ ?[^<>]*>"

(**[first_non_space string] is the first non-space 
    non-newline char in string*)
let first_non_space string = 
  let len = String.length string in
  let rec first_aux string i = 
    if i >= len then 0 else
      match string.[i] with
      | ' ' 
      | '\n' 
      | '\r'-> first_aux string (i + 1)
      | c -> i
  in
  first_aux string 0

(**[get_url_from_anchor string] is a token representing the tag type and
   url field from a valid anchor tag*)
let get_url_from_anchor string= 
  let regex = Str.regexp "href=" in
  let url_regex = Str.regexp "[^ \n\"]*" in
  try 
    let hrefpos = Str.search_forward regex string 0 in 
    let urlpos = String.index_from string hrefpos '"' in
    if Str.string_match url_regex string (urlpos + 1)
    then Opening ("a", Str.matched_string string)
    else Illegal_opening ("a")
  with 
  | Not_found -> Illegal_opening ("a")
  | Invalid_argument s -> Illegal_opening ("a")

(**[get_type_from_token string] is the tag type of tag [string]
   e.g. <p> -> p
   preconditions: [string] must be a valid html tag*)
let get_type_from_token string = 
  let regex = Str.regexp "[a-z0-9]+" in
  if Str.string_match regex string 1
  then Str.matched_string string
  else if Str.string_match regex string 2
  then Str.matched_string string
  else ""

(**[get_token string] is a lexerhelper type with 
   fields: [token] as the type token representation of the first token in the 
            string [tail] as the rest of the string to be parsed
   preconditions: [string] is well-formed html*)
let get_token string =
  let i = first_non_space string in
  if Str.string_match anchor_open_regex string i
  then {token = get_url_from_anchor (Str.matched_string string);
        lextail = (Str.replace_first anchor_open_regex "" string)} 
  else if Str.string_match legal_opening_regex string i 
  then {token = Opening 
            (string |> Str.matched_string |> get_type_from_token, "");
        lextail = (Str.replace_first legal_opening_regex "" string)}
  else if Str.string_match legal_closing_regex string i
  then {token = Closing 
            (string |> Str.matched_string |> get_type_from_token);
        lextail = (Str.replace_first legal_closing_regex "" string)}
  else if Str.string_match illegal_opening_regex string i
  then {token = Illegal_opening 
            (string |> Str.matched_string |> get_type_from_token);
        lextail = (Str.replace_first illegal_opening_regex "" string)}
  else if Str.string_match illegal_closing_regex string i
  then {token = Illegal_closing
            (string |> Str.matched_string |> get_type_from_token);
        lextail = (Str.replace_first illegal_closing_regex "" string)}
  else if Str.string_match comment_regex string i
  then {token = Comment;
        lextail = (Str.replace_first comment_regex "" string)}
  else if Str.string_match word_regex string i
  then {token = Word (Str.matched_string string);
        lextail = (Str.replace_first word_regex "" string)}
  else {token = Illegal_token; lextail = ""}

(**[get_tok_list string] is a lexed tokenized representation of string
    precondtions: [string] is well-formed html*)
let get_tok_list string = 
  let rec lex_aux string acc = 
    match string with
    | "" -> List.rev acc
    | s -> let helper = get_token s in
      lex_aux helper.lextail (helper.token::acc)
  in
  lex_aux string []

(**[lex string] is a pre-processed and lexed token list of string
    preconditions: [string] is well-formed html*)
let lex string = 
  string 
  |> preprocess
  |> get_tok_list

(**[toks_to_string tokens] is a concatenated string representation of tokens
   [toks_to_string] only concatenates Word tokens*)
let toks_to_string tokens = 
  let rec str_aux tokens acc =
    match tokens with
    | [] -> acc |> String.concat " "
    | (Word w)::t -> str_aux t (w::acc)
    | _::t -> str_aux t acc
  in
  str_aux tokens []

(**[tag_builder stack link contents] is the type tag representation
    from args stack, link, contents *)
let tag_builder stack link contents = 
  { name = Stack.pop stack;
    hyperlink = link;
    contents = toks_to_string contents }

(**[format_list_item li] is \n::-::[li], or "-li"
    preconditions: List.hd li is the first element of an [li] tag*)
let format_list_item li =
  let rec get_contents li = 
    match li with
    | [] -> []
    | Word w::t -> 
      let prefix = "\n-" in
      Word (prefix ^ w)::t
    | _::t -> get_contents t
  in
  List.rev (get_contents (List.rev li))

(**[handle_lists stack link tokens] is a listhelper representation of the
   list tag in tokens; handles one-deep recursive list tags
   preconditions: List.hd tokens is the first token after an opening list tag*)
let handle_lists stack link tokens =
  let rec handle_lists_aux stack link tokens acc liacc =
    match tokens with
    | [] -> {tag = tag_builder stack link (List.flatten acc);
             listtail = []}
    | Closing "ol"::t
    | Closing "ul"::t -> {tag = tag_builder stack link (List.flatten acc);
                          listtail = t}
    | Opening ("li", l)::t -> handle_lists_aux stack link t acc []
    | Closing ("li")::t -> 
      handle_lists_aux stack link t ((format_list_item liacc)::acc) []
    | Word w::t -> handle_lists_aux stack link t acc (Word w::liacc)
    | Opening (_, _)::t 
    | Closing (_)::t 
    | Comment::t 
    | Illegal_opening (_)::t 
    | Illegal_closing (_)::t 
    | Illegal_token::t -> handle_lists_aux stack link t acc liacc
  in
  handle_lists_aux stack link tokens [] []

(**[parse tokens] is a parsed type t representation of 
    token list [tokens]*)
let parse tokens =
  let rec parse_aux (tokens : token list) master_acc tag_acc stack link = 
    match tokens with
    | [] -> List.rev master_acc
    | Opening ("a", hyperlink)::t ->
      if Stack.is_empty stack then
        (Stack.push "a" stack; 
         parse_aux t master_acc [] stack hyperlink)
      else let tag = tag_builder stack link tag_acc 
        in parse_aux t (tag::master_acc) [] stack hyperlink
    | Opening ("ol", l)::t
    | Opening ("ul", l)::t ->
      (Stack.push "ul" stack);
      let helper = handle_lists stack link t in
      parse_aux helper.listtail (helper.tag::master_acc) [] stack link
    | Opening (tag, hyperlink)::t -> 
      (Stack.push tag stack);
      parse_aux t master_acc tag_acc stack ""
    | Closing tag::t -> 
      if (Some tag) = (Stack.top_opt stack)
      then let tag = tag_builder stack link tag_acc in
        parse_aux t (tag::master_acc) [] stack link
      else parse_aux t master_acc tag_acc stack link
    | Word w::t -> parse_aux t master_acc (Word w::tag_acc) stack link
    | Comment::t -> parse_aux t master_acc tag_acc stack link
    | Illegal_opening w::t
    | Illegal_closing w::t -> parse_aux t master_acc tag_acc stack link
    | Illegal_token::t -> parse_aux t master_acc tag_acc stack link
  in
  let stack = Stack.create () in
  parse_aux tokens [] [] stack ""

(**[lex_and_parse string] is a lexed and parsed type t representation of 
   [string]*)
let lex_and_parse string =
  string 
  |> lex
  |> parse

(* functions for unit testing *)

let flatten_tags (tags : t) =
  let rec append_tags (tags : t) (acc : string ) =
    match tags with
    | [] -> acc
    | h::t -> append_tags t ((acc) ^ " " ^ ((h.name) ^ " " ^ (h.contents)))
  in
  append_tags tags ""

let to_assoc file = 
  let tags = lex_and_parse file in 
  let rec to_assoc_aux acc = function
    | [] -> acc
    | h :: t -> to_assoc_aux ((h.name, h.contents, h.hyperlink) :: acc) t in
  List.rev(to_assoc_aux [] tags)
