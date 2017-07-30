(* taken from https://caml.inria.fr/mantis/view.php?id=5183 *)

(** [shuffle a] shuffles an array, giving a uniform random distribution *)
let shuffle a =
  for i = pred (Array.length a) downto 1 do
    let j = Random.int (succ i) in
    if i <> j (* faster to omit this test with arrays of about 100000 elements or more *) then (
      let tmp = Array.unsafe_get a i in
      Array.unsafe_set a i (Array.unsafe_get a j);
      Array.unsafe_set a j tmp
    )
  done

let shuffle_and_cut deck =
  let a = Array.of_list deck in
  shuffle a;
  Array.sub a 0 26 |> Array.to_list, 
  Array.sub a 26 26 |> Array.to_list
