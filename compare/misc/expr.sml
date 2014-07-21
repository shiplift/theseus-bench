structure Expressions =
struct

datatype expr = NUM of int
              | Expr of expr
              | Num of expr
              | Add of expr * expr option
              | Mul of expr * expr option;;

exception ParseError of char list;;

fun is_digit x = (x >= #"0") andalso (x <= #"9")
fun int_of_digit x = (ord x) - (ord #"0")

fun relevant_char #" "  = false
  | relevant_char #"\n" = false
  | relevant_char #"\t" = false
  | relevant_char _ = true

fun number l =
    let
        fun loop acc (x::xs) = if is_digit x then
                                   loop (10 * acc + (int_of_digit x)) xs
                               else ((NUM(acc)), x::xs)
          | loop acc []      = ((NUM(acc)), [])
    in
        loop 0 l
    end


fun num_expr (#"("::xs) =
    let
        val (x, l) = expr xs
    in
        case l of
            (#")"::r) => (Num(x), r)
          | _         => raise (ParseError(l))
    end
  | num_expr (x::xs) =
    let
        val (n, r) = number (x::xs)
    in
        (Num(n), r)
    end
  | num_expr l = raise (ParseError(l))
and add_expr l =
    let
        val (x, r) = num_expr l
    in
        case r of
            (#"+"::xs) =>
            let
                val (y, r2) = num_expr xs
            in
                (Add(x, SOME y), r2)
            end
          | _ => (Add(x, NONE), r)
    end
and mul_expr l =
    let
        val (x, r) = add_expr l
    in
        case r of
            (#"*"::xs) =>
            let
                val (y, r2) = add_expr xs
            in
                (Mul(x, SOME y), r2)
            end
          | _ => (Mul(x, NONE), r)
    end
and expr l =
    let
        val (x, r) = mul_expr l
    in
        (Expr (x), r)
    end


fun evaluate (NUM(x)        ) = x
  | evaluate (Expr(x)       ) = evaluate (x)
  | evaluate (Num(x)        ) = evaluate (x)
  | evaluate (Add(x, NONE)  ) = evaluate (x)
  | evaluate (Add(x, SOME y)) = evaluate (x) + evaluate (y)
  | evaluate (Mul(x, NONE)  ) = evaluate (x)
  | evaluate (Mul(x, SOME y)) = evaluate (x) * evaluate (y)

fun transform l = l


fun parse l =
    let
        val (x, r) = expr l
    in
        case r of
            [] => x
          | _  => raise (ParseError(r))
    end

fun print_tree (NUM(x)        ) = print ("NUM(" ^ (Int.toString x) ^ ")")
  | print_tree (Expr(x)       ) = (print ("Expr("); print_tree (x);
                                   print (")"))
  | print_tree (Num(x)        ) = (print ("Num("); print_tree (x); print (")"))
  | print_tree (Add(x, NONE)  ) = (print ("Add("); print_tree (x); print (")"))
  | print_tree (Add(x, SOME y)) = (print ("Add("); print_tree (x);
                                   print (", ");   print_tree (y); print (")"))
  | print_tree (Mul(x, NONE)  ) = (print ("Add("); print_tree (x); print (")"))
  | print_tree (Mul(x, SOME y)) = (print ("Add("); print_tree (x);
                                   print (", ");   print_tree (y); print (")"))

fun input_from_args []      = TextIO.inputAll TextIO.stdIn
  | input_from_args (s::xs) =
    let
        val inchar = TextIO.openIn s
        val chrs = TextIO.inputAll inchar
        val _ = TextIO.closeIn inchar
    in
        chrs
    end

fun main (prog_name, args) =
    let
        val char_list = List.filter relevant_char (
                explode (input_from_args args))
        (* val _ = print ((implode char_list) ^ "\n") *)
        val st = parse char_list
        val _ = (print_tree st; print "\n")
        val ast = transform st
        val _ = (print_tree ast; print "\n")
        val value = evaluate ast
        val _ = (print ((Int.toString value) ^ "\n"))
    in
        0
    end
    handle ParseError l => (
        print ("Parse error at '" ^ (implode l) ^ "'\n"); 1)

end
