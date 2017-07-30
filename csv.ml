open War

let print_step rounds stake (p1, p2) =
  (* sprintf since we don't want to buffer until the end *)
  Printf.sprintf "%d,%d,%d"
    rounds
    (List.length p1)
    (List.length p2)
  |> print_endline


let () =
  Random.self_init ();
  random `With_jokers |> play print_step |> ignore
