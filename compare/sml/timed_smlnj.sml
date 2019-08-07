structure Timed =
struct

fun time_string t = Real.toString ((Time.toReal t) * 1000.0);
fun print_time (what, time) = print ("0:RESULT-" ^ what ^ ":ms: " ^ (time_string time) ^ "\n");

local
  val gettime' :
      unit -> (Int32.int * int * Int32.int * int * Int32.int * int) =
      Unsafe.CInterface.c_function "SMLNJ-Time" "gettime"

  fun mkTime (s, us) = Time.fromMicroseconds (1000000 * Int32.toLarge s + Int.toLarge us)
in
  fun rusage () = 
    let
      val (ts, tu, ss, su, gs, gu) = gettime' ()
    in {
      self  = { utime = mkTime (ts, tu), stime = mkTime (ss, su) },
      gc    = { utime = mkTime (gs, gu), stime = Time.zeroTime }
    } end
end (* local *)

fun timed (action, arg) = 
  let
    val b = Time.now ()
    val {gc = {utime = b_gcu, stime = b_gcs, ...},
         self = {utime = b_selfu, stime = b_selfs}, ...} =
      rusage ()
    val res = action arg
    val {gc = {utime = a_gcu, stime = a_gcs, ...},
         self = {utime = a_selfu, stime = a_selfs}, ...} =
      rusage ()
    val a = Time.now ()
    val _ = print_time ("total", (Time.- (a, b)))
    val cpu = Time.- (Time.+ (a_selfu, a_selfs), Time.+ (b_selfu, b_selfs))
    val _ = print_time ("cpu", cpu)
    val gc = Time.- (Time.+ (a_gcu, a_gcs), Time.+ (b_gcu, b_gcs))
    val _ = print_time ("gc", gc)
  in
    res
  end;
  
end
