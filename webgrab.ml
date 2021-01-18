open Curl
(* 

opam install ocurl

*)
let is_web_extension ext =
  match ext with
  | ".html"
  | ".htm"
  | ".cfm" -> true
  | s -> false

let separate_extension file =
  let pos = String.rindex file '.' in
  let len = String.length file in
  String.sub file pos (len - pos)

let is_file url =
  let fileregex = Str.regexp ".*\\.[a-z]+\\/[^.]+\\.[a-zA-Z1-9_-]+$" in
  if Str.string_match fileregex url 0 |> not 
  then false
  else url 
       |> separate_extension 
       |> is_web_extension 
       |> not


(**[get_filename url] is the name of the file pointed to by [url]

   {b Requires:} [url] is a valid url*)
let get_filename url = 
  let pos = String.rindex url '/' in
  let len = String.length url in
  String.sub url (pos + 1) (len - pos - 1)

(**[download_writefunction filename str] is a writefunction for Curl that
   writes the contents of [str]
   to disk at [filename] in the same directory

   {b Requires:} [filename] is a valid file name and format*)
let download_writefunction filename str=
  let channel = open_out_bin filename in 
  let bytes = Bytes.of_string str in
  output_bytes channel bytes;
  String.length str


(**[access_writefunction buffer str] is  a writefunction for Curl that 
    modifies returns [buffer] containing the contents of the website 
    contents [str]*)
let access_writefunction buffer str= 
  Buffer.add_string buffer str;
  String.length str


(**[choose_writefunction download buffer filename] is [download_writefunction]
   if flag [download] is [true] and [access_writefunction] otherwise*)
let choose_writefunction download buffer filename =
  if download 
  then download_writefunction filename
  else access_writefunction buffer

let rec webgrab_aux debug download filename url = 
  let connection = Curl.init () in 
  try 
    let buffer = Buffer.create 1763 in
    Curl.set_writefunction connection 
      (choose_writefunction download buffer filename);
    Curl.set_url connection url;
    Curl.perform connection;
    let httpcode = Curl.get_httpcode connection in
    if httpcode = 301 || httpcode = 302
    then handle_redirects debug download filename url connection
    else
      let raw_html = 
        (Curl.cleanup connection;
         Buffer.contents buffer;) in
      if debug then "found website"
      else if download then "<p>File successfully downloaded.</p>"
      else raw_html
  with _ -> 
    Curl.cleanup connection;
    if debug
    then "did not find website"
    else "<p>Oh no! Cannot not find the website.</p>"


(** [handle_redirects connection] is a call to webgrab with
    the redirected url one layer deeper

    requires: connection is a performed connection that has returned 
              a http code of 301 or 302
              connection has a valid redirect url*)
and handle_redirects debug download filename url connection =
  let red_url = Curl.get_redirecturl connection in
  Curl.cleanup connection;
  webgrab_aux debug download filename red_url

let webgrab ?debug:(debug=false) url =
  let download = is_file url in
  let filename = 
    if download
    then get_filename url
    else ""
  in
  webgrab_aux debug download filename url
