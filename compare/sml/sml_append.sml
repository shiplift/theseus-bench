structure SMLAppend =
struct

datatype 'a element = E of 'a;
datatype 'a Lst = Nil | Cons of 'a * 'a Lst;
exception Empty;

datatype ('a, 'b) box = Box of 'a * 'b;

val e = E([false]);

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
          | aux acc n = aux (cons e acc) (n - 1)
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
        val m = (make_list num)
        val res = Timed.timed (sml_append1, (Box (l, m)))
        val _ = Int.toString (len res)
    in
        0
    end
    handle Error => let val _ = print "Error.\n" in 1 end
end
