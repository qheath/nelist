(** Non-empty list. *)
type 'a t

(** {6 Creation} *)

val init : int -> (int -> 'a) -> 'a t

val push : 'a -> 'a t option -> 'a t

val push_back : 'a t option -> 'a -> 'a t

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

val binop : ('a -> 'a -> 'a) -> 'a t -> 'a

(** {6 Destruction} *)

val ends : 'a t -> 'a * 'a

val iter :
  ('a -> unit) ->
  ('a -> 'a -> unit) ->
  'a t -> unit

val pop : 'a t -> 'a * 'a t option

val pop_back : 'a t -> 'a t option * 'a

val to_list : 'a t -> 'a list

val length : 'a t -> int
