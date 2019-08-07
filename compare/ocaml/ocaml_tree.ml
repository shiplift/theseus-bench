(* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * Contributed by Troestler Christophe
 * Modified by Fabrice Le Fessant for
 *  - tree type more compact
 *  - better GC parameters
 *  - loops replaced by recursive functions
 * Adapted for lamb-bench by Tobias Pape
 *)

type 'a element = E of 'a | F of 'a ;;

type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

let e = E([false])
and f = F([false])
;;

let rec make d e =
  if d = 0 then Leaf e
  else
    let l = make (d-1) e in
    let r = make (d-1) e in
      Node(l, e, r)


let rec iter i niter d =
  if i <= niter then
    let _ = make d e in
    let _ = make d f in
    iter (i+1) niter d
  (* else *)
  (*   Printf.printf "%i\t trees of depth %i\n" (2 * niter) d *)

let min_depth = 3

let rec loop_depths d max_depth =
  let niter = 1 lsl (max_depth - d) in
  let _ = iter 1 niter d in
  if d < max_depth then
    loop_depths (d+1) max_depth

let rec check = function
  | Leaf el -> e
  | Node(l, el, r) ->
     let _ = check l in
     check r

let ocaml_tree num =
  let max_depth = num in
  let stretch_depth = max_depth + 1 in
  let _ = make stretch_depth e in
  let long_lived_tree = make max_depth e in
  let _ = loop_depths min_depth max_depth in
  check long_lived_tree

let time f x =
  let tot_start = Unix.gettimeofday () in
  let cpu_start = Sys.time() in
  let res = f x in
  let cpu_stop = Sys.time() in
  let tot_stop = Unix.gettimeofday () in
  let () = Printf.printf "0:RESULT-cpu:ms: %f\n0:RESULT-total:ms: %f\n"
                         ((cpu_stop -. cpu_start) *. 1000.0)
                         ((tot_stop -. tot_start) *. 1000.0)
  let () = Printf.printf "0:RESULT-gc:ms: %f\n" 0.0
  in
  res

let treenum a =
  try
    (min_depth - 1) + int_of_string (Array.get a 1) / 1_000_000
  with _ -> 18

let _ =
  let num = (treenum Sys.argv) in
  let _ = time ocaml_tree num in
  0
;;
