type expr = NUM of int
          | Expr of expr
          | Num of expr
          | Add of expr * expr option
          | Mul of expr * expr option;;
(* Concrete tree parts *)
type cexpr = CNUM of int
           | CAdd of cexpr * cexpr
           | CMul of cexpr * cexpr;;

exception ParseError of char list;;
exception TransformError of expr;;

let rec implode = function
  | [] -> ""
  | x::xs -> (String.make 1 x) ^ (implode xs)

let rec print_tree = function
  | NUM(x)         -> Printf.printf "NUM(%i)" x
  | Num(x)         -> Printf.printf "Num("; print_tree (x); Printf.printf ")";
  | Expr(x)        -> Printf.printf "Expr("; print_tree (x); Printf.printf ")";
  | Add(x, None)   -> Printf.printf "Add("; print_tree (x); Printf.printf ")";
  | Add(x, Some y) -> Printf.printf "Add("; print_tree (x);
                      Printf.printf ", ";   print_tree (y); Printf.printf ")"
  | Mul(x, None)   -> Printf.printf "Mul("; print_tree (x); Printf.printf ")";
  | Mul(x, Some y) -> Printf.printf "Mul("; print_tree (x);
                      Printf.printf ", ";   print_tree (y); Printf.printf ")"

let rec print_ctree = function
  | CNUM(x)        -> Printf.printf "%i" x
  | CAdd(x, y)     -> Printf.printf "("; print_ctree (x);
                      Printf.printf " + ";   print_ctree (y); Printf.printf ")"
  | CMul(x, y)     -> Printf.printf "("; print_ctree (x);
                      Printf.printf " * ";   print_ctree (y); Printf.printf ")"

let relevant_char = function
  | ' ' | '\n' | '\t' -> false
  | _ -> true

let is_digit x = (x >= '0') && (x <= '9')
let int_of_digit x = (int_of_char x) - (int_of_char '0')

let number l =
  let rec loop acc = function
    | x::xs when is_digit x -> loop (10 * acc + (int_of_digit x)) xs
    | l -> ((NUM(acc)), l)
  in
  loop 0 l


let rec num_expr l = match l with
  | x::xs when is_digit x -> let (n, r) = number l in
                             (Num(n), r)
  | '('::xs -> begin let (x, l) = expr xs in
                     match l with
                     | ')'::r -> (Num(x), r)
                     | _ -> raise (ParseError(l))
               end
  | _ -> raise (ParseError(l))
and add_expr l =
  let (x, l) = num_expr l in
  match l with
  | '+'::xs -> let (y, l) = num_expr xs in
               (Add(x, Some y), l)
  | _ -> (Add(x, None), l)
and mul_expr l =
  let (x, l) = add_expr l in
  match l with
  | '*'::xs -> let (y, l) = add_expr xs in
               (Mul(x, Some y), l)
  | _ -> (Mul(x, None), l)
and expr l =
  let (x, r) = mul_expr l in
  (Expr (x), r)


let rec evaluate = function
  | CAdd(x, y) -> evaluate (x) + evaluate (y)
  | CMul(x, y) -> evaluate (x) * evaluate (y)
  | CNUM(x)    -> x

let rec transform = function
  | Num(NUM(x))     -> CNUM(x)
  | Num(Expr(x))    -> transform (x)
  | Expr(x)         -> transform (x)
  | Add(x, None)    -> transform (x)
  | Add(x, Some y)  -> CAdd(transform (x), transform (y))
  | Mul(x, None)    -> transform (x)
  | Mul(x, Some y)  -> CMul(transform (x), transform (y))
  | x               -> raise (TransformError(x))


let parse l =
  let (x, r) = expr l in
  match r with
  | [] -> x
  | _  -> raise (ParseError(r))


let rec read inchar char_list =
  try
    read inchar ((input_char inchar)::char_list)
  with End_of_file -> List.rev char_list

let time f x =
  let tot_start = Unix.gettimeofday () in
  let cpu_start = Sys.time() in
  let res = f x in
  let cpu_stop = Sys.time() in
  let tot_stop = Unix.gettimeofday () in
  let () = Printf.printf "0:RESULT-cpu:ms: %f\n0:RESULT-total:ms: %f\n"
                         ((cpu_stop -. cpu_start) *. 1000.0)
                         ((tot_stop -. tot_start) *. 1000.0)
  in
  res

let _ =
  try
    let chars = match (Array.length Sys.argv) with
      | 1 -> read stdin []
      | _ -> let inchar = open_in Sys.argv.(1) in
             let c = read inchar [] in
             let () = close_in inchar in
             c
    in
    let char_list = List.filter relevant_char chars in
    let st = parse char_list in
    (* let () = print_tree st; Printf.printf "\n" in *)
    let ast = time transform st in
    let () = print_ctree ast; Printf.printf "\n" in
    let value = evaluate ast in
    let () = Printf.printf "%i\n" value in
    0
  with ParseError(l) -> (Printf.printf "Parse error at '%s'\n" (implode l); 1);;
