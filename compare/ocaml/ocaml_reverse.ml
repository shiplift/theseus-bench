type element = E ;;

type 'a lst = Nil | Cons of  'a * 'a lst ;;
exception Empty;;

let cons a b = Cons(a, b) ;;

let head = function
    Nil -> raise Empty
  | Cons(a, b) -> a
;;
let tail = function
    Nil -> raise Empty
  | Cons(a, b) -> b
;;


let ocaml_reverse list =
    let rec aux acc = function
      | Nil -> acc
      | (Cons(h,t)) -> aux (Cons (h, acc)) t in
    aux Nil list;;

(* let ocaml_reverse list = *)
(*     let rec aux acc = function *)
(*       | [] -> acc *)
(*       | h::t -> aux (h::acc) t in *)
(*     aux [] list;; *)

let make_list num =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (Cons (E, acc)) (n - 1) in
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
  let () = Printf.printf "RESULT-cpu: %f\nRESULT-total: %f\n"
                         ((cpu_stop -. cpu_start) *. 1000.0)
                         ((tot_stop -. tot_start) *. 1000.0)
  in
  res

let _ =
  let l = (make_list 20000000) in
  let _ = time ocaml_reverse l in
  0
;;
