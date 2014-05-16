type element = E | F ;;

type 'a lst = Nil | Cons of  'a * 'a lst ;;
exception Empty;;

type 'a box = Box of (element -> bool) * 'a;;

let cons a b = Cons(a, b) ;;

let head = function
    Nil -> raise Empty
  | (Cons(a, b)) -> a
;;
let tail = function
    Nil -> raise Empty
  | (Cons(a, b)) -> b
;;


let rec ocaml_filter p = function
  | Nil -> Nil
  | (Cons (h, t)) -> if p h then cons h (ocaml_filter p t) else ocaml_filter p t;;

(* let rec ocaml_filter p = function *)
(*   | [] -> [] *)
(*   | x :: xs -> if p x then x :: (ocaml_filter p xs) else ocaml_filter p xs;; *)

let ocaml_filter1 (Box(p,l)) = ocaml_filter p l;;

let make_list num =
  let rec aux acc = function
    | 0 -> acc
    | n -> let elem =
             if (n mod 2) = 0 then E else F
           in aux (Cons (elem, acc)) (n - 1) in
  aux Nil num;;



(* let make_list num = *)
(*   let rec aux acc = function *)
(*     | 0 -> acc *)
(*     | n -> let elem = *)
(*              if (n mod 2) = 0 then E else F *)
(*            in aux (elem :: acc) (n - 1) in *)
(*   aux [] num;; *)

let time f x =
  let tot_start = Unix.gettimeofday () in
  let cpu_start = Sys.time() in
  let res = f x in
  let cpu_stop = Sys.time() in
  let tot_stop = Unix.gettimeofday () in
  let () = Printf.printf "RESULT-cpu: %f\nRESULT-total: %f\n"
                         ((cpu_stop -. cpu_start) *. 1000.0)
                         ((tot_stop -. tot_start) *. 1000.0)
  in
  res

let len lst =
  let rec aux acc = function
    | Nil -> acc
    | (Cons (h, t)) -> aux (acc + 1) t
  in
  aux 0 lst

let flt = function
  | E -> true
  | F -> false
;;

(**)

let listnum a =
  try
    int_of_string (Array.get a 1)
  with _ -> 5000000

let _ =
  let num = (listnum Sys.argv) in
  let l = (make_list num) in
  let res = time ocaml_filter1 (Box (flt, l)) in
  let _ = (len res) in
  0
;;
