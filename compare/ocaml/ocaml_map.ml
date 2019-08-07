type 'a element = E of 'a | F of 'a ;;

type 'a lst = Nil | Cons of  'a * 'a lst ;;
exception Empty;;

type ('a, 'b) box = Box of 'a * 'b;;

let cons a b = Cons(a, b) ;;

let head = function
    Nil -> raise Empty
  | (Cons(a, b)) -> a
;;
let tail = function
    Nil -> raise Empty
  | (Cons(a, b)) -> b
;;

let e = E([false])
and f = F([false])
;;


(* let rec ocaml_map f = function *)
(*   | [] -> [] *)
(*   | h::t -> (f h) :: (ocaml_map f t);; *)

let rec ocaml_map f = function
  | Nil -> Nil
  | (Cons(h, t)) -> cons (f h) (ocaml_map f t);;

let ocaml_map1 (Box(f,l)) = ocaml_map f l;;

let make_list num =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (Cons (e, acc)) (n - 1) in
  aux Nil num;;



(* let make_list num = *)
(*   let rec aux acc = function *)
(*     | 0 -> acc *)
(*     | n -> aux (E :: acc) (n - 1) in *)
(*   aux [] num;; *)

let time f x =
  let tot_start = Unix.gettimeofday () in
  let cpu_start = Sys.time() in
  let res = f x in
  let cpu_stop = Sys.time() in
  let tot_stop = Unix.gettimeofday () in
  let () = Printf.printf "0:RESULT-cpu:ms: %f\n0:RESULT-total:ms: %f\n"
                         ((cpu_stop -. cpu_start) *. 1000.0)
                         ((tot_stop -. tot_start) *. 1000.0) in
  let () = Printf.printf "0:RESULT-gc:ms: %f\n" 0.0
  in
  res

let swap x = if (x = e) then f else e;;

let listnum a =
  try
    int_of_string (Array.get a 1)
  with _ -> 5000000

let _ =
  let num = (listnum Sys.argv) in
  let l = (make_list num) in
  let _ = time ocaml_map1 (Box (swap, l)) in
  0
;;
