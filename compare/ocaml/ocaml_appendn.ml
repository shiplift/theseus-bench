

type 'a lst = Nil | Cons of  'a * 'a lst ;;
exception Empty;;

type 'a box = Box of 'a * 'a;;

let cons a b = Cons(a, b) ;;

let head = function
    Nil -> raise Empty
  | Cons(a, b) -> a
;;
let tail = function
    Nil -> raise Empty
  | Cons(a, b) -> b
;;

let e = 17
;;

(* let rec ocaml_append a b = *)
(*   match a with *)
(*   | [] -> b *)
(*   | x::xs -> x :: (ocaml_append xs b) *)

let rec ocaml_append a b =
  match a with
  | Nil -> b
  | Cons(x, xs) -> cons x (ocaml_append xs b)

(* let ocaml_append a b = *)
(*     let rec aux acc = function *)
(*         | [] -> acc *)
(*         | x::xs -> aux (x::acc) xs *)
(*     loop b (List.rev a) *)

(* let ocaml_append a b = *)
(*     let rec aux acc = function *)
(*         | Nil -> acc *)
(*         | (Cons(h,t)) -> aux (Cons(h, acc)) t *)
(*     in *)
(*     aux b (ocaml_reverse a) *)

let ocaml_append1 = function
    Box(a,b) -> ocaml_append a b

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

let listnum a =
  try
    int_of_string (Array.get a 1)
  with _ -> 10000000

let _ =
  let num = (listnum Sys.argv) in
  let l = (make_list num) in
  let m = (make_list num) in
  let _ = time ocaml_append1 (Box (l, m)) in
  0
;;
