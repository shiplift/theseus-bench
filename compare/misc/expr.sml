structure Expressions =
struct

datatype expr = NUM of int
              | Expr of expr
              | Num of expr
              | Add of expr * expr option
              | Mul of expr * expr option;;

(* Concrete tree parts *)
datatype cexpr = CNUM of int
               | CAdd of cexpr * cexpr
               | CMul of cexpr * cexpr;;

exception ParseError of char list;;
exception TransformError of expr;;

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


fun evaluate (CNUM(x)        ) = x
  | evaluate (CAdd(x, y)     ) = evaluate (x) + evaluate (y)
  | evaluate (CMul(x, y)     ) = evaluate (x) * evaluate (y)

fun transform (Num(NUM(x))    ) = CNUM(x)
  | transform (Num(Expr(x))   ) = transform (x)
  | transform (Expr(x)        ) = transform (x)
  | transform (Add(x, NONE)   ) = transform (x)
  | transform (Add(x, SOME y) ) = CAdd(transform (x), transform (y))
  | transform (Mul(x, NONE)   ) = transform (x)
  | transform (Mul(x, SOME y) ) = CMul(transform (x), transform (y))
  | transform (x              ) = raise (TransformError(x))


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
  | print_tree (Mul(x, NONE)  ) = (print ("Mul("); print_tree (x); print (")"))
  | print_tree (Mul(x, SOME y)) = (print ("Mul("); print_tree (x);
                                   print (", ");   print_tree (y); print (")"))

fun print_ctree (CNUM(x)       ) = (print (Int.toString x))
  | print_ctree (CAdd(x, y)    ) = (print ("(");  print_ctree (x);
                                   print (" + "); print_ctree (y); print (")"))
  | print_ctree (CMul(x, y)    ) = (print ("(");  print_ctree (x);
                                   print (" * "); print_ctree (y); print (")"))

fun input_from_args []      = TextIO.inputAll TextIO.stdIn
  | input_from_args (s::xs) =
    let
        val inchar = TextIO.openIn s
        val chrs = TextIO.inputAll inchar
        val _ = TextIO.closeIn inchar
    in
        chrs
    end


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

fun main (prog_name, args) =
    let
        val char_list = List.filter relevant_char (
                explode (input_from_args args))
        (* val _ = print ((implode char_list) ^ "\n") *)
        val st = parse char_list
        (*val _ = (print_tree st; print "\n")*)
        val ast = time ((prog_name = "mlton"), transform, st)
        val _ = (print_ctree ast; print "\n")
        (* val value = evaluate ast *)
        (* val _ = (print ((Int.toString value) ^ "\n")) *)
    in
        0
    end
    handle ParseError l => (
        print ("Parse error at '" ^ (implode l) ^ "'\n"); 1)

end
