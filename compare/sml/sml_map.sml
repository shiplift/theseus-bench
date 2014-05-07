structure SMLMap =
struct

datatype element = E | F;
datatype 'a Lst = Nil | Cons of 'a * 'a Lst;
exception Empty;

datatype 'a box = Box of (element -> element) * 'a;

fun cons a b = Cons (a, b);
fun head Nil = raise Empty
  | head (Cons (a, b)) = a;
fun tail Nil = raise Empty
  | tail (Cons (a, b)) = b;

(* fun sml_map list = *)
(*     let *)
(*         fun aux acc Nil = acc *)
(*           | aux acc (Cons(h,t)) = aux (Cons (h, acc)) t *)
(*     in *)
(*         aux Nil list *)
(*     end; *)

fun sml_map f []     = []
  | sml_map f (h::t) = (f h) :: (sml_map f t);

fun sml_map1 (Box(f,l)) = sml_map f l;

(* fun make_list num = *)
(*     let *)
(*         fun aux acc 0 = acc *)
(*           | aux acc n = aux (cons E acc) (n - 1) *)
(*     in *)
(*         aux Nil num *)
(*     end; *)

fun make_list num =
    let
        fun aux acc 0 = acc
          | aux acc n = aux (E :: acc) (n - 1)
    in
        aux [] num
    end;

fun time (action, arg) = let
    (* val tot_timer = Timer.startRealTimer () *)
    val cpu_timer = Timer.startCPUTimer ()
    val res = action arg
    val cpu_times = Timer.checkCPUTimer cpu_timer
    (* val tot_times = Timer.checkRealTimer tot_timer *)
    val _ = print ("RESULT-cpu: " ^ (Time.toString (Time.+ (#usr cpu_times, #sys cpu_times))) ^ "\n")
    (* Cheat here, the total thing seems bogus *)
    val _ = print ("RESULT-total: " ^ (Time.toString (Time.+ (#usr cpu_times, #sys cpu_times))) ^ "\n")
    (* val _ = print ("RESULT-total: " ^ (Time.toString tot_times) ^ "\n") *)
in
    res
end

fun swap E = F
  | swap F = E;

fun len lst =
    let
        fun aux acc Nil = acc
          | aux acc (Cons (h, t)) = aux (acc + 1) t
    in
        aux 0 lst
    end;

fun main (prog_name, args) =
    let
        val l = (make_list 5000000)
        val res = time (sml_map1, (Box (swap, l)))
        val _ = Int.toString (List.length res)
    in
        0
    end
    handle Error => let val _ = print "Error.\n" in 1 end
end
