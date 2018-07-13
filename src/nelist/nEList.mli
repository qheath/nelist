(** Non-empty list. *)
type 'a t

(** {6 Creation} *)

val make : 'a -> 'a t

val init : int -> (int -> 'a) -> 'a t

val push : 'a -> 'a t -> 'a t

val of_list : 'a list -> 'a t option

(** {6 Manipulation/transformation} *)

val rev_append : 'a list -> 'a t -> 'a t

val fold :
  ('a -> 'start -> 'finish) ->
  ('a -> 'finish -> 'start) ->
  'a t -> 'start -> 'finish

val rev_map : ('a -> 'b) -> 'a t -> 'b t

val rev : 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val flatten : 'a t t -> 'a t

(** {6 Destruction} *)

val ends : 'a t -> 'a * 'a

val iter :
  ('a -> unit) ->
  ('a -> 'a -> unit) ->
  'a t -> unit

val pop : 'a t -> ('a * 'a t) option

val to_list : 'a t -> 'a list

val length : 'a t -> int
