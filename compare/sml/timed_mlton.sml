structure Timed =
struct

fun time_string t = Real.toString ((Time.toReal t) * 1000.0);
fun print_time (what, time) = print ("0:RESULT-" ^ what ^ ":ms: " ^ (time_string time) ^ "\n");

fun timed (action, arg) = 
  let
    val _ = MLton.Rusage.measureGC true
    val b = Time.now ()
    val {gc = {utime = b_gcu, stime = b_gcs, ...},
         self = {utime = b_selfu, stime = b_selfs}, ...} =
      MLton.Rusage.rusage ()
    val res = action arg
    val {gc = {utime = a_gcu, stime = a_gcs, ...},
         self = {utime = a_selfu, stime = a_selfs}, ...} =
      MLton.Rusage.rusage ()
    val a = Time.now ()
    val total = Time.- (a, b)
    val cpu = Time.- (Time.+ (a_selfu, a_selfs), Time.+ (b_selfu, b_selfs))
    val gc = Time.- (Time.+ (a_gcu, a_gcs), Time.+ (b_gcu, b_gcs))
    val _ = print_time ("cpu", cpu)
    val _ = print_time ("total", total)
    val _ = print_time ("gc", gc)
  in
    res
  end;
  
end
