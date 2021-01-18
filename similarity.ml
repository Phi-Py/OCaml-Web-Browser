(** [|i1, i2, i3, ... in|] is the feature representation
     of datum d

     No invariants*)

type t = float list

let empty = 
  []

let from_list lst =
  lst

let to_list v =
  v

(** [to_char_list str] is the char list representation of [str]
     "str" -> ['s';'t';'r']*)
let to_char_list str =
  let len = String.length str in
  let rec char_aux str i acc = 
    match i with
    | -1 -> acc
    | j -> char_aux str (j - 1) (str.[j]::acc)
  in
  char_aux str (len - 1) [] 

(**[get_char_index c] is the index of the one hot vector representation
    of c
    eg: a -> 0
        b -> 1
        ...
        z -> 25
        ' ' -> 26
        with all else -> 27*)
let get_char_index c =
  let i = c 
          |> Char.lowercase_ascii
          |> int_of_char 
  in
  match i - 97 with
  | -65 -> 26
  | i when i >= 0 && i <= 25 -> i
  | i -> 27

(** [to_char_vector str] is the linear combination of one-hot
    char vectors for each char in [str] with a..z = 0..25, ' ' = 26
    and all other = 27
    requires: none*)
let char_vector str = 
  let v = Array.make 28 0. in
  let rec vector_aux chars =
    match chars with
    | [] -> ()
    | h::t -> 
      let i = get_char_index h in
      v.(i) <- v.(i) +. 1.;
      vector_aux t
  in
  let chars = to_char_list str in
  vector_aux chars;
  Array.to_list v

(** [extract_features data] is a feature vector from the data in [data]*)
let extract_features ?vec_type:(vec_type="char") data =
  if vec_type = "char"
  then char_vector data
  else failwith "unimplemented"

let add v w =
  List.map2 ( +. ) v w

let mult v w =
  List.map2 ( *. ) v w

let dot v w =
  mult v w
  |> List.fold_left ( +. ) 0.

let norm v = 
  let rec square_list acc lst =
    match lst with
    | [] -> List.rev acc
    | h::t -> square_list ((Float.pow h 2.)::acc) t
  in
  let norm = 
    v
    |> square_list []
    |> List.fold_left ( +. ) 0.
    |> Float.sqrt
  in 
  if norm = 0. 
  then 1.
  else norm

let sim v w = 
  let dot_vw = dot v w
  and norm_v = norm v  
  and norm_w = norm w in
  dot_vw /. (norm_v *. norm_w)


