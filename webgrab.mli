(** Performs GET requests to the internet
    and receives source [html]*)


(**[webgrab url] is the html or file at [url] iff [url] is a valid url and 
    an error string if not.

    If [url] points to a non-web file, the file is downloaded.

    {b Requires:} [url] is a valid html*)            
val webgrab : ?debug:bool -> string -> string