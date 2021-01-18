(**Feature vector operations. Contains functions for transforming to
   feature vectors as well. *)


(** Specialized feature vector that uses similarity calculations *)
type t

(** [empty] is an empty webvector *)
val empty : t

(** [from_list lst] is the vector representation of [lst]*)
val from_list : float list -> t

(** [to_list v] is the list representation of [v]*)
val to_list : t -> float list

(** [char_vector str] is a feature vector consisting of 
    the linear combinations of allone hot char vectors of 
    the chars in [string]*)
val char_vector : string -> t

(** [add v1 v2] is the element-wise sum of the two vectors*)
val add : t -> t -> t

(** [dot v w] is the dot product of the two vectors*)
val dot : t -> t -> float

(** [norm v] is the norm of [v]*)
val norm : t -> float

(** [sim v w] is the cosine similarity between [v] and [w]*)
val sim : t -> t -> float