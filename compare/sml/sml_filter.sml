structure SMLFilter =
struct

datatype element = E | F;
datatype 'a Lst = Nil | Cons of 'a * 'a Lst;
exception Empty;

datatype 'a box = Box of (element -> bool) * 'a;

fun cons a b = Cons (a, b);
fun head Nil = raise Empty
  | head (Cons (a, b)) = a;
fun tail Nil = raise Empty
  | tail (Cons (a, b)) = b;


fun sml_filter p []     = []
  | sml_filter p (h::t) = if (p h) then h :: (sml_filter p t) else sml_filter p t;

fun sml_filter1 (Box(p,l)) = sml_filter p l;

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
          | aux acc n = let
              val elem = if (n mod 2) = 0 then E else F
          in
              aux (elem :: acc) (n - 1)
          end
    in
        aux [] num
    end;

fun time_string t =
    Real.toString ((Time.toReal t) * 1000.0);


fun time (mlton, action, arg) = let
    val tot_timer = Timer.startRealTimer ()
    val cpu_timer = Timer.startCPUTimer ()
    val res = action arg
    val cpu_times = Timer.checkCPUTimer cpu_timer
    val tot_times = Timer.checkRealTimer tot_timer
    val _ = print ("RESULT-cpu: " ^ (time_string (Time.+ (#usr cpu_times, #sys cpu_times))) ^ "\n")
    val _ = if mlton then
                print ("RESULT-total: " ^ (time_string tot_times) ^ "\n")
            else
                (* Cheat here, the total thing seems bogus on SML/NJ *)
                print ("RESULT-total: " ^ (time_string (Time.+ (#usr cpu_times, #sys cpu_times))) ^ "\n")
in
    res
end



fun flt E = true
  | flt F = false;

fun len lst =
    let
        fun aux acc Nil = acc
          | aux acc (Cons (h, t)) = aux (acc + 1) t
    in
        aux 0 lst
    end;

fun listnum []      = 10000000
  | listnum (x::xs) = Option.valOf (Int.fromString x);

fun main (prog_name, args) =
    let
        val num = listnum (args)
        val l = (make_list num)
        val res = time ((prog_name = "mlton"), sml_filter1, (Box (flt, l)))
        val _ = Int.toString (List.length res)
    in
        0
    end
    handle Error => let val _ = print "Error.\n" in 1 end
end
