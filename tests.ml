(** OVERALL TEST STRATEGY *)
(** because a significant portion of the project is GUI-based, we were not
    able to test all of our modules with OUnit tests. Below is a bulleted 
    list of which modules were tested with OUnit and which ones were tested
    visually:

    • Main: Visually
    • GUI: Visually
    • Saved: Visually
    • Webgrab: OUnit2/Visually
    • Parse: OUnit2
    • Similarity: OUnit2
    • Style: Ounit2

    A more detailed explanation for each follows:
    Main:       For Main, every function is concerned with opening the GUI as 
                as outlined in Gui. Without a complicated and lengthy conversion
                function, testing with OUnit is not possible. Even with one,
                it would not be desirable as the test would be complex enough
                to introduce concerns that it was written incorrectly and not 
                accurately testing the module. So, we used visual testing.
                We tested the browser under a wide variety of contitions
                in an attempt to make it break, and each time it did 
                we isolated the problem and fixed it. This provides us with 
                high confidence that the module was implemented correctly.

    Gui:        For GUI, same applies
    
    Saved:      The specification and implementation of Saved is simple
                and it didn't require extensive testing. Since Saved
                handles bookmarks and history, we also tested it visually
                in concert with Main and GUI, and since it is a simple 
                "ferry" from the browser to external saved state,
                we are reasonably sure it is correct.

    Webgrab:    We could have tested Webgrab solely with OUnit, but that would
                have required very lengthy expected_output strings, so we went
                with a hybrid glass-box method. We used OUnit to test whether
                the module, given a url, would find the website or not. The 
                optional parameter ?debug assisted with this, as it allowed
                the function to map valid urls to a success string and invalid
                ones to a failure string (valid and invalid here meaning one
                that cdoes not raise an exception). The visual component
                of the module testing was testing if the web redirect worked.
                Instead of designing arcane use of the ?debug param to return 
                a correct number of redirects output given an arbitrary possible
                amount, we inspected cases visually to determine if they were
                redirecting to the correct page. We can be sure of the 
                correctness of this funciton as a webpage only shows its full
                raw html if the url is exact, otherwise it shows some 
                variation of "document moved" and perhaps a link to the exact
                url. 

    Parse:      Parse is a deterministic fully-functional module, which makes it
                ideal for OUnit testing. We tested a wide variety of 
                corner cases and non-corner cases using glass-box testing; due
                to the expressiveness of html, we could not ensure adequate
                coverage using black-box testing. In addition, since the parser 
                is  not a true html parser, we needed knowledge of internal 
                implementation to craft text; we needed to know how it handled 
                malformed html, handled links and lists, etc. This was our 
                most-tested module due to its centrality to the browser, and 
                this allows us high confidence that it is correct.

    Similarity: Similarity is essentially a mini-vector mathematics module
                which makes it perfect to test with OUnit. Type Similarity.t
                is immutable, which allowed us to do black-box testing
                as there was no worry of non-determinism in the functions.

    Style:      The majority of style is color and style definitions that do not 
                need to be tested. The functions that did need to be tested we
                tested with OUnit, as the module is purely functional. To
                ensure that all request types were tested, we used 
                a glass-box testing strategy, particularly for the user
                color request, to ensure there were no untested paths that could
                cause the browser to crash while handling a user style request.*)


open OUnit2
open Parse
open Webgrab
open Similarity


(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [pp_string s] pretty-prints [s] as "s" *)
let pp_string s = 
  "\"" ^ s ^ "\""

(** [pp_float fl] pretty-prints [fl] *)
let pp_float fl = 
  string_of_float fl

(** [pp_parse elt] prett-prints a Parse.t element*)
let pp_parse elt =
  let x,y,z = elt in
  let ppx = pp_string x
  and ppy = pp_string y
  and ppz = pp_string z in
  "("^ ppx ^ ", " ^ ppy ^ ", " ^ ppz ^ ")"

(** [string_printer] is a printer for string ounit testing*)
let string_printer : string -> string = 
  pp_string

(** [vec_printer] is a printer for float list ounit testing *)
let vec_printer : float list -> string =
  pp_list (pp_float)

(** [parse_printer] is a printer for testing the Parse module*)
let parse_printer : (string * string * string) list -> string = 
  pp_list (pp_parse)

let parse_test
    (name : string)
    (string : string)
    (expected_output : (string * string * string) list) =
  name >:: (fun _ -> 
      assert_equal
        expected_output (to_assoc string)
        ~printer:parse_printer)

let parse_tests =[
  (** well-formed html tests *)
  parse_test
    "basic empty"
    ""
    [];
  parse_test
    "one tag"
    "<p> this is my website </p>"
    [("p", "this is my website", "")];
  parse_test
    "one tag with no space separation"
    "<p>this is my website</p>"
    [("p", "this is my website", "")];
  parse_test
    "one illegal tag"
    {|<img src="img.jpg">alt-text</img>|}
    [];
  parse_test
    "one tag nested in div"
    {|<div class="main">
      <p>this is my website</p>
    </div>|}
    [("p", "this is my website", "")];
  parse_test
    "two tags"
    {|<h1>Title</h1><p>Lorem ipsum...</p>|}
    [("h1", "Title", ""); ("p", "Lorem ipsum...", "")];
  parse_test
    "two tags inside div inside html"
    {|<html>
        <div class="foo">
          <h1>Header</h1>
          <p>Lorem ipsum...</p>
        </div>
      </html>|}
    [("h1", "Header", ""); ("p", "Lorem ipsum...", "")];
  parse_test
    "header and body"
    {|<html>
      <head>
        <link href="style.css rel="stylesheet">
        <link href="google font">
        <title>Test</title>
      </head>
      <body>
        <h1>Title</h1>
        <p>Paragraph 1</p>
        <p>Paragraph 2</p>
      </body>
    </html>|}
    [("h1", "Title", ""); ("p", "Paragraph 1", ""); ("p", "Paragraph 2", "")];
  parse_test
    "mix of legal and illegal tags"
    {|<html>
        <head>
          <link href="./style.css" rel="stylesheet">
          <link href="google font api ex">
          <title>Title</title>
        </head>
        <body>
          <div class="main">
          <h1>Header</h1>
          <p>Paragraph</p>
          <img src="image.jpg">
            Alt text
            </img>
          </div>
        </body>
      </html>|}
    [("h1", "Header", ""); ("p", "Paragraph", "");];
  parse_test
    "illegal tag nested in legal tag"
    {|<p>Lorem <img src="img.jpg"></img> Ipsum</p>|}
    [("p", "Lorem Ipsum", "")];
  parse_test
    "anchor tag"
    {|<a href="www.google.com">Link</a>|}
    [("a", "Link", "www.google.com")];
  parse_test
    "partial link"
    {|<a class="menu-item" href="/about" data-click-label="About Cornell">
    About Cornell</a>|}
    [ ("a", "About Cornell", "/about")];
  parse_test
    "no closing tag"
    {|<p>this paragraph does not have a closing tag|}
    [];
  parse_test
    "nested legal tags"
    {|<p>this is paragraph 1 <p> but it is interrupted</p>|}
    [("p", "this is paragraph 1 but it is interrupted", ""); ];
]

let webgrab_test 
    (name : string)
    (url : string)
    (expected_output : string) =
  name >:: (fun _ -> 
      assert_equal expected_output (webgrab ~debug:true url) 
        ~printer:string_printer)

let webgrab_tests = [
  webgrab_test 
    "fully-well-formed url"
    "http://www.google.com" 
    "found website";
  webgrab_test
    "one redirect url"
    "www.google.com"
    "found website";
  webgrab_test
    "two redirect url"
    "cs.cornell.edu/courses/cs3110/2020sp"
    "found website";
  webgrab_test
    "empty url"
    ""
    "did not find website";
  webgrab_test
    "ill-formed url"
    "www google com"
    "did not find website";
]

let gui_test 
    (name : string)
    (url : string)
    (expected_output : string) =
  name >:: (fun _ -> 
      assert_equal expected_output (webgrab url) )

let saved_tests = [

]

let char_vector_test 
    (name : string)
    (str : string)
    (expected_output : float list) =
  name >:: (fun _ ->
      let retlst = 
        str 
        |> char_vector
        |> to_list in
      assert_equal expected_output retlst
        ~printer:vec_printer)

let char_vector_tests = [
  char_vector_test
    "empty string"
    ""
    [0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;
     0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.];
  char_vector_test
    "simple no-space string"
    "ocaml"
    [1.;0.;1.;0.;0.;0.;0.;0.;0.;0.;0.;1.;1.;0.;
     1.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.];
  char_vector_test
    "simple duplicated char string no spaces"
    "google"
    [0.;0.;0.;0.;1.;0.;2.;0.;0.;0.;0.;1.;0.;0.;
     2.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.];
  char_vector_test
    "simple string with spaces"
    "ocaml vs python"
    [1.;0.;1.;0.;0.;0.;0.;1.;0.;0.;0.;1.;1.;1.;
     2.;1.;0.;0.;1.;1.;0.;1.;0.;0.;1.;0.;2.;0.];
  char_vector_test
    "simple string with capital letters"
    "OCaml"
    [1.;0.;1.;0.;0.;0.;0.;0.;0.;0.;0.;1.;1.;0.;
     1.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.];
  char_vector_test
    "non-letter/space characters"
    "oc4ml"
    [0.;0.;1.;0.;0.;0.;0.;0.;0.;0.;0.;1.;1.;0.;
     1.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;1.];
  char_vector_test
    "all special cases combined"
    "OC4ML >> everything else"
    [0.;0.;1.;0.;4.;0.;1.;1.;1.;0.;0.;2.;1.;1.;
     1.;0.;0.;1.;1.;1.;0.;1.;0.;0.;1.;0.;3.;3.];
]

let add_vector_test 
    (name : string)
    (lst1 : float list)
    (lst2 : float list)
    (expected_output : float list) =
  name >::       
  (let v1 = from_list lst1
   and v2 = from_list lst2 in
   let retlst = (Similarity.add v1 v2) |> to_list in
   (fun _ -> 
      assert_equal expected_output retlst
        ~printer:vec_printer))

let add_vector_tests = [
  add_vector_test
    "empty vectors"
    []
    []
    [];
  add_vector_test
    "one-element vector"
    [2.]
    [2.]
    [4.];
  add_vector_test
    "multi-element vector"
    [1.;2.;3.]
    [3.;2.;1.]
    [4.;4.;4.];
]

let dot_test
    (name : string)
    (lst1 : float list)
    (lst2 : float list)
    (expected_output : float) =
  name >::       
  (let v1 = from_list lst1
   and v2 = from_list lst2 in
   let ret = (Similarity.dot v1 v2) in
   (fun _ -> 
      assert_equal expected_output ret))

let dot_tests = [
  dot_test
    "empty vectors"
    []
    []
    0.;
  dot_test
    "one element vectors"
    [2.]
    [3.]
    6.;
  dot_test
    "two element vectors"
    [1.;2.]
    [2.;6.]
    14.;
  dot_test
    "long vectors"
    [1.;2.;8.;5.;3.;5.;9.;0.]
    [1.;5.;8.;1.;9.;6.;2.;5.]
    155.;
]

let norm_test
    (name : string)
    (lst : float list)
    (expected_output : float) =
  name >:: 
  (let vec = Similarity.from_list lst in
   (fun _ -> 
      assert_equal expected_output (Similarity.norm vec)))

let norm_tests = [
  norm_test
    "empty vector"
    []
    1.;
  norm_test
    "non-empty zero vector"
    [0.;0.;0.;0.;0.]
    1.;
  norm_test
    "non-zero vector"
    [3.;4.;]
    5.;
]

let similarity_test
    (name : string)
    (lst1 : float list)
    (lst2 : float list)
    (expected_output : float) = 
  name >::
  (let v1 = Similarity.from_list lst1
   and v2 = Similarity.from_list lst2 in
   let retsim = Similarity.sim v1 v2 in
   (fun _ ->
      assert_equal expected_output retsim))

let sim_tests = [
  similarity_test
    "empty vectors should not throw error"
    []
    []
    0.;
  similarity_test
    "zero vectors should not throw divide by zero"
    [0.;0.;0.;0.]
    [0.;0.;0.;0.]
    0.;
  similarity_test
    "one zero vector should be 0"
    [0.;0.;0.;0.]
    [1.;4.;2.;6.]
    0.;
  similarity_test
    "identical vectors should be 1"
    [1.;2.;3.;4.]
    [1.;2.;3.;4.]
    1.;
  similarity_test
    "orthogonal vectors should be 0"
    [1.;0.]
    [0.;1.]
    0.;
  similarity_test
    "non-orthogonal vectors"
    [0.;3.;2.;6.]
    [1.;3.;7.;3.]
    0.710282901892118;
]

let get_color_test
    (name : string)
    (input : string)
    (expected_output : Graphics.color) =
  name >:: (fun _ ->
      assert_equal expected_output (Style.get_color input))

let get_color_tests = [
  get_color_test
    "exact match builtin"
    "blue"
    Graphics.blue;
  get_color_test
    "exact match custom"
    "lightgoldenrodyellow"
    (Graphics.rgb 250 250 210);  
  get_color_test
    "rgb test"
    "255 255 255"
    (Graphics.rgb 255 255 255);
  get_color_test
    "rgb test"
    "30 145 9"
    (Graphics.rgb 30 145 9);
  get_color_test
    "test too long input, should truncate first three"
    "50 100 150 200"
    (Graphics.rgb 50 100 150);
  get_color_test
    "test too long input with illegal input, should truncate"
    "50 100 150 200 250 300 350 10000000000"
    (Graphics.rgb 50 100 150);
  get_color_test
    "illegal rgb values should return white"
    "125 256 42"
    Graphics.white;  
  get_color_test
    "illegal rgb values on other side should return white"
    "-23 42 2"
    Graphics.white;
  get_color_test
    "similarity test with long string and inserted spaces"
    "light goldenrod yellow"
    (Graphics.rgb 250 250 210);
  get_color_test
    "similarity test with mispelling"
    "kaki"
    (Graphics.rgb 240 230 140);
]

let build_style_test
    (name : string)
    (lst : string list)
    (expected_output : Style.style) =
  name >:: (fun _ ->
      assert_equal expected_output (Style.build_style lst))

let build_style_tests = [
  build_style_test
    "style of all color literals"
    ["blue";"blue";"blue";"blue";"blue";"blue";"blue"]
    { 
      navcolor = Graphics.blue;
      navtextcolor = Graphics.blue;
      backgroundcolor = Graphics.blue;
      headercolor = Graphics.blue;
      subheadcolor = Graphics.blue;
      listcolor = Graphics.blue;
      textcolor = Graphics.blue;
    };
  build_style_test
    "style with non-literals"
    ["yellow";"golden";"light golden rod yellow";"black";
     "navajo";"crimson";"red"]
    { 
      navcolor = Graphics.yellow;
      navtextcolor = Graphics.rgb 218 165 32;
      backgroundcolor = Graphics.rgb 250 250 210;
      headercolor = Graphics.black;
      subheadcolor = Graphics.rgb 255 222 173;
      listcolor = Graphics.rgb 220 20 60;
      textcolor = Graphics.red;
    }
]

let suite =
  "test suite for htmlparse"  >::: List.flatten [
    parse_tests;
    webgrab_tests;
    saved_tests;
    char_vector_tests;
    add_vector_tests;
    dot_tests;
    norm_tests;
    sim_tests;
    get_color_tests;
    build_style_tests;
  ]

let _ = run_test_tt_main suite
