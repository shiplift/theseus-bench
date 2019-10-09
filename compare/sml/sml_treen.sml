(* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * Contributed by Troestler Christophe
 * Modified by Fabrice Le Fessant for
 *  - tree type more compact
 *  - better GC parameters
 *  - loops replaced by recursive functions
 * Adapted for lamb-bench and SML by Tobias Pape
 *)
structure SMLTreen =
struct


datatype 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

val e = 17;
val f = 36;


fun make 0 e = Leaf (e)
  | make d e =
    let
        val l = make (d-1) e
        val r = make (d-1) e
    in
        Node(l, e, r)
    end;

fun iter (i, niter, d) =
    if i <= niter then
        let
            val _ = make d e
            val _ = make d f
        in
            iter (i+1, niter, d)
        end
    else ()

val min_depth = 3

fun loop_depths (d, max_depth) =
    let
        val niter = Int.fromLarge (IntInf.pow (2, max_depth - d))
        val _ = iter (1, niter, d)
    in
        if d < max_depth then
            loop_depths (d+1, max_depth)
        else ()
    end;

fun check (Leaf(el)) = e
  | check (Node(l, el, r)) =
    let val _ = check l in check r end;

fun sml_tree num =
    let
        val max_depth = num
        val stretch_depth = max_depth + 1
        val _ = make stretch_depth e
        val long_lived_tree = make max_depth e
        val _ = loop_depths (min_depth, max_depth)
    in
        check long_lived_tree
    end;


fun treenum [] = 18
  | treenum (x::xs) = (min_depth - 1) + (Option.valOf (Int.fromString x) div 1000000);

fun tst x = if (x = e) then f else e;


fun main (prog_name, args) =
    let
        val num = treenum (args)
        val res = Timed.timed (sml_tree, num)
        val _ = Int.toString (tst res)
    in
        0
    end
    handle Error => let val _ = print "Error.\n" in 1 end
end
