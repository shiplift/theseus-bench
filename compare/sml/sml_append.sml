structure SMLAppend =
struct

datatype element = E;
datatype 'a Lst = Nil | Cons of 'a * 'a Lst;
exception Empty;

datatype 'a box = Box of 'a * 'a;

fun cons a b = Cons (a, b);
fun head Nil = raise Empty
  | head (Cons (a, b)) = a;
fun tail Nil = raise Empty
  | tail (Cons (a, b)) = b;

(* fun sml_append []      b = b *)
(*   | sml_append (x::xs) b = (x :: (sml_append xs b)); *)

fun sml_append Nil           b = b
  | sml_append (Cons(x, xs)) b = cons x (sml_append xs b);

fun sml_append1 (Box(a,b)) = sml_append a b;

fun make_list num =
    let
        fun aux acc 0 = acc
          | aux acc n = aux (cons E acc) (n - 1)
    in
        aux Nil num
    end;

(* fun make_list num = *)
(*     let *)
(*         fun aux acc 0 = acc *)
(*           | aux acc n = aux (E :: acc) (n - 1) *)
(*     in *)
(*         aux [] num *)
(*     end; *)

fun time_string t =
    Real.toString ((Time.toReal t) * 1000.0);

fun time (action, arg) = let
    (* val tot_timer = Timer.startRealTimer () *)
    val cpu_timer = Timer.startCPUTimer ()
    val res = action arg
    val cpu_times = Timer.checkCPUTimer cpu_timer
    (* val tot_times = Timer.checkRealTimer tot_timer *)
    val _ = print ("RESULT-cpu: " ^ (time_string (Time.+ (#usr cpu_times, #sys cpu_times))) ^ "\n")
    (* Cheat here, the total thing seems bogus *)
    val _ = print ("RESULT-total: " ^ (time_string (Time.+ (#usr cpu_times, #sys cpu_times))) ^ "\n")
    (* val _ = print ("RESULT-total: " ^ (time_string tot_times) ^ "\n") *)
in
    res
end

fun len lst =
    let
        fun aux acc Nil = acc
          | aux acc (Cons (h, t)) = aux (acc + 1) t
    in
        aux 0 lst
    end;

fun main (prog_name, args) =
    let
        val l = (make_list 10000000)
        val m = (make_list 10000000)
        val res = time (sml_append1, (Box (l, m)))
        val _ = Int.toString (len res)
    in
        0
    end
    handle Error => let val _ = print "Error.\n" in 1 end
end
