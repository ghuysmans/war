type suit = Diamonds | Clubs | Hearts | Spades
val suits : suit list
val string_of_suit : suit -> string

type value = Pip of int | Jack | Queen | King | Ace
val string_of_value : value -> string
val pips : value list
val faces : value list
val values : value list

type card
val string_of_card : card -> string
val ( <= ) : card -> card -> bool
val whole_suit : suit -> card list
val whole_deck : card list

(** [t] represents a game *)
type t
(** [random jokers] creates a new random game *)
val random : [< `With_jokers | `Without_jokers ] -> t

(** [play step game] plays war step by step until completion *)
val play :
  (int -> card list -> card list * card list -> unit) ->
  t ->
  int * bool
