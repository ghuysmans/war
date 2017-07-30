type suit =
  | Diamonds
  | Clubs
  | Hearts
  | Spades

let suits = [Diamonds; Clubs; Hearts; Spades]

let string_of_suit = function
  | Diamonds -> "♦"
  | Clubs -> "♣"
  | Hearts -> "♥"
  | Spades -> "♠"

type value =
  | Pip of int
  | Jack
  | Queen
  | King
  | Ace

let string_of_value = function
  | Pip i -> string_of_int i
  | Jack -> "V"
  | Queen -> "D"
  | King -> "R"
  | Ace -> "A"

let pips = Array.init 9 (fun x -> Pip (x + 2)) |> Array.to_list
let faces = [Jack; Queen; King; Ace]
let values = pips @ faces

type card =
  | Regular of suit * value
  | Joker

let string_of_card = function
  | Joker ->
    "J"
  | Regular (suit, value) ->
    string_of_value value ^ string_of_suit suit

let (<=) x y = match (x, y) with
  | _, Joker ->
    true
  | Joker, _ ->
    (* this works since match is sequential... *)
    false
  | Regular (_, x), Regular (_, y) ->
    (* easier than matching values by hand *)
    let int_of_value = function
      | Pip i -> i
      | Jack -> 11
      | Queen -> 12
      | King -> 13
      | Ace -> 14 in
    int_of_value x <= int_of_value y

let whole_suit suit =
  values |> List.map (fun v -> Regular (suit, v))

let whole_deck =
  suits |> List.map whole_suit |> List.concat

type t = card list * card list

let random jokers =
  let jokers = match jokers with
    | `With_jokers -> [Joker; Joker]
    | `Without_jokers -> [] in
  jokers @ whole_deck |> Rand.shuffle_and_cut

let play debug =
  let rec f round stake game =
    debug round stake game;
    match game with
    (* base cases (we could return what the winner has, but we don't care) *)
    | [], _ -> round, false
    | _, [] -> round, true
    (* inductive case *)
    | x::r, y::s ->
      let stake = x :: y :: stake in
      let round = round + 1 in
      if x <= y && y <= x then
        f round stake (r, s)
      else if x <= y then (* x < y *)
        f round [] (r, s @ stake)
      else (* x > y *)
        f round [] (r @ stake, s) in
  f 1 []
