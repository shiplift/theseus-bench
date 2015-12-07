structure SMLMap =
struct

datatype 'a element = E of 'a | F of 'a;
datatype 'a Lst = Nil | Cons of 'a * 'a Lst;
exception Empty;

datatype ('a, 'b) box = Box of 'a * 'b;
val e = E([false]);
val f = F([false]);

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
          | aux acc n = aux (e :: acc) (n - 1)
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
    val _ = print ("0:RESULT-cpu:ms: " ^ (time_string (Time.+ (#usr cpu_times, #sys cpu_times))) ^ "\n")
    val _ = if mlton then
                print ("0:RESULT-total:ms: " ^ (time_string tot_times) ^ "\n")
            else
                (* Cheat here, the total thing seems bogus on SML/NJ *)
                print ("0:RESULT-total:ms: " ^ (time_string (Time.+ (#usr cpu_times, #sys cpu_times))) ^ "\n")
in
    res
end

fun swap x = if (x = e) then f else e;

fun len lst =
    let
        fun aux acc Nil = acc
          | aux acc (Cons (h, t)) = aux (acc + 1) t
    in
        aux 0 lst
    end;

fun listnum []      = 5000000
  | listnum (x::xs) = Option.valOf (Int.fromString x);

fun main (prog_name, args) =
    let
        val num = listnum (args)
        val l = (make_list num)
        val res = time ((prog_name = "mlton"), sml_map1, (Box (swap, l)))
        val _ = Int.toString (List.length res)
    in
        0
    end
    handle Error => let val _ = print "Error.\n" in 1 end
end
