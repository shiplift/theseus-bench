structure SMLFiltern =
struct


datatype 'a Lst = Nil | Cons of 'a * 'a Lst;
exception Empty;

datatype ('a, 'b) box = Box of 'a * 'b;

val e = 17;
val f = 36;

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
              val elem = if (n mod 2) = 0 then e else f
          in
              aux (elem :: acc) (n - 1)
          end
    in
        aux [] num
    end;





fun flt x = (x = e);

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
        val res = Timed.timed (sml_filter1, (Box (flt, l)))
        val _ = Int.toString (List.length res)
    in
        0
    end
    handle Error => let val _ = print "Error.\n" in 1 end
end
