(** Browser colors and styles *)


(**[Similarity.t] represented as a [Map] compatible comparable *)
module Vector : sig 
  type t

  (**[compare v1 v2] compares [v1] and [v2] by norm*)
  val compare : t -> t -> int
end


(**Represents browser styles. Exposed to allow use by the GUI. *)
type style = { 
  navcolor : Graphics.color;
  navtextcolor : Graphics.color;
  backgroundcolor : Graphics.color;
  headercolor : Graphics.color;
  subheadcolor : Graphics.color;
  listcolor : Graphics.color;
  textcolor : Graphics.color;
}


(**The default browser style *)
val default : style

(**Style with grays and dark blues *)
val dark : style

(**Style with a light grey and blue scheme*)
val light : style

(**Style with a dark and neon green scheme*)
val hacker : style

(**Style with a tan and light blue and green scheme *)
val desert : style

(**Style with high contrast beige and black scheme *)
val minimalist : style

(**Style with deep blue and bright yellow scheme *)
val powershell : style

val test : style

(**List of prebuilt styles *)
val styles : string list

(** [get_color str] is the [Graphics.color] representation of str

    {b Requires:} [str] is either an html color label or
              [str] = "x y z" where 0 <= x,y,z <= 255*)  
val get_color : string -> Graphics.color

val build_style : string list -> style

