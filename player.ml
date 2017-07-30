open War

(* not used anymore, but... *)
let string_of_heads = function
  | [], x::_ -> "-\t" ^ string_of_card x
  | x::_, [] -> string_of_card x ^ "\t-"
  | x::_, y::_ -> string_of_card x ^ "\t" ^ string_of_card y
  | [], [] ->
    (* just for exhaustiveness: cards can't just disappear,
     * but the compiler is unable to prove it... yet? *)
    assert false

let first = function
  | [] -> None
  | x::_ -> Some x

let map f = function
  | Some x -> Some (f x)
  | None -> None

let default d = function
  | Some x -> x
  | None -> d

(* FIXME use an external module to handle UTF8 *)
let left_pad c n s =
  let l = String.length s in
  if l < n then
    String.make (n - l) c ^ s
  else
    s

let string_of_head l =
  l |> first |> map string_of_card |> default "---" (* |> left_pad ' ' 3 *)

let print_step rounds stake (p1, p2) =
  let string_of_stake l =
    List.map string_of_card l |> String.concat " " in
  (* sprintf since we don't want to buffer until the end *)
  Printf.sprintf "%d\t%s\t%d\t%s\t%d\t%s"
    rounds
    (string_of_head p1)
    (List.length p1)
    (string_of_head p2)
    (List.length p2)
    (string_of_stake stake)
  |> print_endline;
  (* slower pace *)
  Unix.sleep 1

let print_result (rounds, p1_has_won) =
  let winner = if p1_has_won then "Player 1" else "Player 2" in
  Printf.printf "%s has won in %d rounds.\n" winner rounds


let () =
  Random.self_init ();
  random `With_jokers |> play print_step |> print_result
