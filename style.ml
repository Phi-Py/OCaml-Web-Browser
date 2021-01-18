open Graphics
open Similarity

module Vector =
struct
  type t = Similarity.t

  let compare v1 v2 = 
    let diff = Similarity.norm v1 -. Similarity.norm v2 in
    if diff < 0. 
    then -1
    else if diff = 0.
    then 0
    else 1   
end

(** XY is the map f : names -> values *)
module XY = Map.Make(String)

(**ZX is the map f : vectors -> names*)
module ZX = Map.Make(Vector)

(**[Illegal_rgb] is raised when system tries to create colors 
   with invalid rgb values*)
exception Illegal_rgb 

(**represents of the style of a webpage *)
type style = {
  navcolor : Graphics.color;
  navtextcolor : Graphics.color;
  backgroundcolor : Graphics.color;
  headercolor : Graphics.color;
  subheadcolor : Graphics.color;
  listcolor : Graphics.color;
  textcolor : Graphics.color;
}

(**represents a user color request*)
type color_request =
  | Name of string
  | RGB of int * int * int

(** the 140 html color labels supported by modern browsers 
    all available to be selected in custom styles by the user *)
(* reds *)
let rust = rgb 205 92 29

let coral	= rgb 240 128 128

let salmon = rgb 250 128 114

let crimson	= rgb 220 20 60

let firebrick	= rgb 178 34 34

let darkred	= rgb 139 0 0


(* pinks *)
let pink = rgb 255 192 203

let lightpink =	rgb 255 182 193

let hotpink = rgb 255 105 180

let deeppink = rgb 255 20 147

let mediumvioletred = rgb 199 21 133

let palevioletred = rgb 219 112 147


(* oranges *)
let tomato = rgb 255 99 71

let orangered = rgb 255 69 0

let darkorange = rgb 255 140 0

let orange = Graphics.rgb 255 165 0

(* yellows *)
let gold = rgb 255 215 0

let lightyellow = rgb 255 255 224

let lemonchiffon = rgb 255 250 205

let lightgoldenrodyellow = rgb 250 250 210

let papayawhip = rgb 255 239 213

let moccasin = rgb 255 228 181

let peachpuff = rgb 255 218 185

let palegoldenrod = rgb 238 232 170

let khaki	= rgb 240 230 140

let darkkhaki = rgb 189 183 107

(* purples *)
let lavender = rgb 230 230 250

let thistle	= rgb 216 191 216

let plum = rgb 221 160 221

let violet = rgb 238 130 238

let orchid = rgb 218 112 214

let fuchsia = rgb 255 0 255

let magenta	= rgb 255 0 255

let mediumorchid = rgb 186 85 211

let mediumpurple = rgb 147 112 219

let rebeccapurple = rgb 102 51 153

let blueviolet = rgb 138 43 226

let darkviolet = rgb 148 0 211

let darkorchid = rgb 153 50 204

let darkmagenta	= rgb 139 0 139

let purple = rgb 128 0 128

let indigo = rgb 75 0 130

let slateblue = rgb 106 90 205

let darkslateblue	= rgb 72 61 139

let mediumslateblue = rgb 123 104 238

(* greens *)
let greenyellow	= rgb 173 255 47

let chartreuse = rgb 127 255 0

let lawngreen	= rgb 124 252 0

let lime = rgb 0 255 0

let limegreen = rgb 50 205 50

let palegreen = rgb 152 251 152

let lightgreen = rgb 144 238 144

let mediumspringgreen	= rgb 0 250 154

let springgreen	= rgb 0 255 127

let mediumseagreen = rgb 60 179 113

let seagreen = rgb 46 139 87

let forestgreen = rgb 34 139 34

let darkgreen	= rgb 0 100 0

let yellowgreen	= rgb 154 205 50

let olivedrab	= rgb 107 142 35

let olive	= rgb 128 128 0

let darkolivegreen = rgb 85 107 47

let mediumaquamarine = rgb 102 205 170

let darkseagreen = rgb 143 188 139

let lightseagreen = rgb 32 178 170

let darkcyan = rgb 0 139 139

let teal = rgb 0 128 128

(* blues *)
let aqua = rgb 0 255 255

let aquamarine = rgb 127 255 212

let turquoise	= rgb 64 224 208

let cadetblue = rgb 95 158 160

let steelblue = rgb 70 130 180

let powderblue = rgb 176 224 230

let skyblue = rgb 135 206 235

let dodgerblue = rgb 30 144 255

let cornflowerblue = rgb 100 149 237

let royalblue = rgb 65 105 225

let navy = rgb 0 0 128

let midnightblue = rgb 25 25 112

(* browns *)
let cornsilk = rgb 255 248 220

let blanchedalmond = rgb 255 235 205

let bisque = rgb 255 228 196

let navajowhite	= rgb 255 222 173

let wheat = rgb 245 222 179

let burlywood	= rgb 222 184 135

let tan	= rgb 210 180 140

let rosybrown = rgb 188 143 143

let sandybrown = rgb 244 164 96

let goldenrod	= rgb 218 165 32

let peru = rgb 205 133 63

let chocolate = rgb 210 105 30

let saddlebrown	= rgb 139 69 19

let sienna = rgb 160 82 45

let brown	= rgb 165 42 42

let maroon = rgb 128 0 0

(* whites *)
let snow = rgb 255 250 250

let honeydew = rgb 240 255 240

let mintcream	= rgb 245 255 250

let azure	= rgb 240 255 255

let aliceblue	= rgb 240 248 255

let ghostwhite = rgb 248 248 255

let whitesmoke = rgb 245 245 245

let seashell = rgb 255 245 238

let beige	= rgb 245 245 220

let oldlace	= rgb 253 245 230

let floralwhite = rgb 255 250 240

let ivory = rgb 255 255 240

let antiquewhite = rgb 250 235 215

let linen = rgb 250 240 230

let lavenderblush	= rgb 255 240 245

let mistyrose = rgb 255 228 225

(* grays *)
let gainsboro	= rgb 220 220 220

let lightgray = rgb 211 211 211

let silver = rgb 192 192 192

let darkgray = rgb 169 169 169

let gray = rgb 128 128 128

let dimgray = rgb 105 105 105

let lightslategray = rgb 119 136 153

let slategray = rgb 112 128 144

let darkslategray = rgb 47 79 79

(* prebuilt styles used by the browser*)

(**The default browser style *)
let default = {
  navcolor = blue;
  navtextcolor = yellow;
  backgroundcolor = black;
  headercolor = cyan;
  subheadcolor = magenta;
  listcolor = magenta;
  textcolor = white;
}

(**Style with grays and dark blues *)
let dark = {
  navcolor = darkgray;
  navtextcolor = midnightblue;
  backgroundcolor = black;
  headercolor = midnightblue;
  subheadcolor = darkmagenta;
  listcolor = darkmagenta;
  textcolor = gray;   
}

(**Style with  *)
let light = {
  navcolor = lightgray;
  navtextcolor = royalblue;
  backgroundcolor = aliceblue;
  headercolor = midnightblue;
  subheadcolor = darkmagenta;
  listcolor = darkmagenta;
  textcolor = black;   
}

let hacker = {
  navcolor = black;
  navtextcolor = green;
  backgroundcolor = black;
  headercolor = green;
  subheadcolor = green;
  listcolor = green;
  textcolor = green;   
}

let desert = {
  navcolor = seashell;
  navtextcolor = springgreen;
  backgroundcolor = wheat;
  headercolor = turquoise;
  subheadcolor = turquoise;
  listcolor = violet;
  textcolor = black;   
}

let minimalist = {
  navcolor = linen;
  navtextcolor = black;
  backgroundcolor = linen;
  headercolor = black;
  subheadcolor = black;
  listcolor = black;
  textcolor = black;   
}

let powershell = {
  navcolor = midnightblue;
  navtextcolor = yellow;
  backgroundcolor = midnightblue;
  headercolor = yellow;
  subheadcolor = yellow;
  listcolor = yellow;
  textcolor = yellow;   
}

let test = {
  navcolor = darkslateblue;
  navtextcolor = white;
  backgroundcolor = white;
  headercolor = darkslateblue;
  subheadcolor = darkslateblue;
  listcolor = darkslateblue;
  textcolor = darkslateblue;   
}

let styles = [
  "default";
  "dark";
  "desert";
  "hacker";
  "light";
  "minimalist";
  "powershell";
]

(** [colorkv] is an assoc list of (colorname, color) used for maps
    later in the module*)
let colorkv = [
  ("black", black); 
  ("white", white); 
  ("red", red); 
  ("green", green); 
  ("blue", blue); 
  ("yellow", yellow); 
  ("cyan", cyan); 
  ("magenta", magenta); 
  ("aliceblue", aliceblue);
  ("antiquewhite", antiquewhite);
  ("aqua", aqua);
  ("aquamarine", aquamarine);
  ("azure", azure);
  ("beige", beige);
  ("bisque", bisque);
  ("blanchedalmond", blanchedalmond);
  ("blueviolet", blueviolet);
  ("brown", brown);
  ("burlywood", burlywood);
  ("cadetblue", cadetblue);
  ("chartreuse", chartreuse);
  ("chocolate", chocolate);
  ("coral", coral);
  ("cornflowerblue", cornflowerblue);
  ("cornsilk", cornsilk);
  ("crimson", crimson);
  ("darkcyan", darkcyan);
  ("darkgray", darkgray);
  ("darkgreen", darkgreen);
  ("dark green", darkgreen);
  ("darkkhaki", darkkhaki);
  ("darkmagenta", darkmagenta);
  ("darkolivegreen", darkolivegreen);
  ("darkorange", darkorange);
  ("darkorchid", darkorchid);
  ("darkred", darkred);
  ("darkseagreen", darkseagreen);
  ("darkslateblue", darkslateblue);
  ("darkslategray", darkslategray);
  ("darkviolet", darkviolet);
  ("deeppink", deeppink);
  ("dimgray", dimgray);
  ("dodgerblue", dodgerblue);
  ("dodger blue"), dodgerblue;
  ("firebrick", firebrick);
  ("floralwhite", floralwhite);
  ("forestgreen", forestgreen);
  ("fuchsia", fuchsia);
  ("gainsboro", gainsboro);
  ("ghostwhite", ghostwhite);
  ("gold", gold);
  ("goldenrod", goldenrod);
  ("gray", gray);
  ("greenyellow", greenyellow);
  ("honeydew", honeydew);
  ("hotpink", hotpink);
  ("indigo", indigo);
  ("ivory", ivory);
  ("khaki", khaki);
  ("lavender", lavender);
  ("lavenderblush", lavenderblush);
  ("lawngreen", lawngreen);
  ("lemonchiffron", lemonchiffon);
  ("lightgoldenrodyellow", lightgoldenrodyellow);
  ("lightgray", lightgray);
  ("lightgreen", lightgreen);
  ("lightpink", lightpink);
  ("lightseagreen", lightseagreen);
  ("lightslategray", lightslategray);
  ("lightyellow", lightyellow);
  ("lime", lime);
  ("limegreen", limegreen);
  ("linen", linen);
  ("maroon", maroon);
  ("mediumaquamarine", mediumaquamarine);
  ("mediumorchid", mediumorchid);
  ("mediumpurple", mediumpurple);
  ("mediumseagreen", mediumseagreen);
  ("mediumslateblue", mediumslateblue);
  ("mediumspringgreen", mediumspringgreen);
  ("mediumvioletred", mediumvioletred);
  ("midnightblue", midnightblue);
  ("midnight", midnightblue);
  ("mintcream", mintcream);
  ("mistyrose", mistyrose);
  ("mocassin", moccasin);
  ("navajowhite", navajowhite);
  ("navy", navy);
  ("oldlace", oldlace);
  ("olive", olive);
  ("olivedrab", olivedrab);
  ("orange", orange);
  ("orangered", orangered);
  ("orchid", orchid);
  ("palegoldenron", palegoldenrod);
  ("palegreen", palegreen);
  ("palevioletred", palevioletred);
  ("papayawhip", papayawhip);
  ("peachpuff", peachpuff);
  ("peru", peru);
  ("plum", plum);
  ("powderblue", powderblue);
  ("rebeccapurple", rebeccapurple);
  ("rosybrown", rosybrown);
  ("royalblue", royalblue);
  ("saddlebrown", saddlebrown);
  ("salmon", salmon);
  ("sandybrown", sandybrown);
  ("seagreen", seagreen);
  ("seashell", seashell);
  ("sienna", sienna);
  ("silver", silver);
  ("skyblue", skyblue);
  ("slateblue", slateblue);
  ("slategray", slategray);
  ("snow", snow);
  ("springgreen", springgreen);
  ("steelblue", steelblue);
  ("tan", tan);
  ("teal", teal);
  ("thistle", thistle);
  ("tomato", tomato);
  ("turquoise", turquoise);
  ("violet", violet);
  ("wheat", wheat);
  ("whitesmoke", whitesmoke);
  ("yellowgreen", yellowgreen);
]


(**[construct_name_map lst] is the map from names to colors

    ie (blue, Graphics.blue) is blue -> Graphics.blue*)
let construct_name_map lst =
  lst
  |> List.to_seq
  |> XY.of_seq 


(** [construct_vector_map lst] is the map from the first elt of each pair

    ie. with input ("abc", x) the map is [vector "abc"] -> "abc"*)
let construct_vector_map lst =
  let rec vector_aux lst map =
    match lst with
    | [] -> map
    | (name, color)::t ->
      let vec = Similarity.char_vector name
      and name = name in
      vector_aux t (ZX.add vec name map)
  in
  vector_aux lst (ZX.empty)


(** name_map maps names to colors*)
let name_map = construct_name_map colorkv


(** vec_map maps char vectors to names*)
let vec_map = construct_vector_map colorkv


(** [truncate n lst] is the first n elts of [lst]

    requires: n >= len lst*)
let truncate n lst =
  let rec trun_aux n lst acc =
    match n, lst with
    | 0, l -> List.rev acc
    | i, [] -> List.rev acc
    | i, h::t -> trun_aux (i - 1) t (h::acc)
  in
  trun_aux n lst []


(**[compare p1 p2] compares with the second elts of [p1] and [p2] descending*)
let compare_pair p1 p2 =
  (-1) * (compare (snd p1) (snd p2))


(** [get_top_sim str lst] is a list of the top 5 most similar elts in [lst]*)
let get_top_sim str lst =
  let rec sim_aux v1 lst acc =
    match lst with
    | [] -> List.rev acc
    | h::t ->
      let v2 = Similarity.char_vector h in
      let sim = Similarity.sim v1 v2 in
      sim_aux v1 t ((h, sim)::acc)
  in
  let v1 = Similarity.char_vector str in
  sim_aux v1 lst []


(**[get_vals lst map] is the list of values for every k in map
    requires: forall (x,y) in [lst], there exists a mapping x -> z in [map]*)
let get_vals lst map =
  let rec get_aux lst map acc =
    match lst with
    | [] -> List.rev acc
    | h::t ->
      let value = XY.find h map in
      get_aux t map (value::acc)
  in
  get_aux lst map []


(**[retrieve n str lst] is the n most similar elts in [lst] to [str]*)
let retrieve n str lst =
  let sims = get_top_sim str lst in
  sims
  |> List.sort compare_pair
  |> truncate n
  |> List.map fst


(** [get_color_string string] is [string] if [string] is a valid colorname and 
    [white] otherwise*)
let get_color_str map str=
  if XY.mem str map
  then XY.find str map
  else
    let names = List.map fst colorkv in
    let raw_sims = retrieve 5 str names in
    let toplst = get_vals raw_sims name_map in
    match List.nth_opt toplst 0 with
    | Some c -> c
    | None -> white


(** [get_color_ints r g b] is the Graphics.color representation
    of rgb(r,g,b)

    requires: 0 <= r,g,b <= 255*)
let get_color_ints r g b = 
  rgb r g b


(** [to_color req] is the Graphics.color representation of request [req]

    requires: if req = RGB r,g,b, 0 <= r,g,b <= 255*)
let to_color map req =
  match req with
  | Name n -> get_color_str map n
  | RGB (r, g, b) -> get_color_ints r g b


(**[to_request str] is the Request type of str

   Requires: [str] is either an html color label or
             [str] = "x y z" where 0 <= x,y,z <= 255*)
let to_request str = 
  let rec req_aux strs acc =
    match acc, strs with
    | b::g::r::[], l -> RGB (r, g, b)
    | l, h::t -> 
      let colorval = int_of_string h in
      if colorval < 0 || colorval > 255 
      then raise Illegal_rgb
      else req_aux t (colorval::acc)
    | l, [] -> raise (Invalid_argument "invalid format")
  in
  let strs = String.split_on_char ' ' str in
  try req_aux strs []
  with
  | Illegal_rgb -> RGB (255, 255, 255) 
  | Failure exn -> Name str


(** [get_color str] is the Graphics.color representation of str

    requires: [str] is either an html color label or
              [str] = "x y z" where 0 <= x,y,z <= 255*)                          
let get_color str =
  str
  |> to_request
  |> to_color name_map


(** [get_colors n str] is the top n similar colors to str*)
let get_colors n str =
  str
  |> to_request
  |> to_color name_map


(**  [get_style lst] is the style formed by the list of colors [lst]
  *  colors in [lst] are applied to style in order, and excess values
  *  are truncated
  *  if [List.length lst < 7], style is set to [default]
  *  
  *  requires: [List.length = 7]*)   
let build_style lst =
  let colorarr = lst |> List.map get_color |> Array.of_list in
  try
    { navcolor = colorarr.(0);
      navtextcolor = colorarr.(1);
      backgroundcolor = colorarr.(2);
      headercolor = colorarr.(3);
      subheadcolor = colorarr.(4);
      listcolor = colorarr.(5);
      textcolor = colorarr.(6);
    }
  with exn -> default