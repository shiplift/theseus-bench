type expr = NUM of int
          | Expr of expr
          | Num of expr
          | Add of expr * expr option
          | Mul of expr * expr option;;

exception ParseError of char list;;

let rec implode = function
  | [] -> ""
  | x::xs -> (String.make 1 x) ^ (implode xs)

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
  | NUM(x)         -> x
  | Expr(x)        -> evaluate (x)
  | Num(x)         -> evaluate (x)
  | Add(x, None)   -> evaluate (x)
  | Add(x, Some y) -> evaluate (x) + evaluate (y)
  | Mul(x, None)   -> evaluate (x)
  | Mul(x, Some y) -> evaluate (x) * evaluate (y)

let transform l =
  l


let parse l =
  let (x, r) = expr l in
  match r with
  | [] -> x
  | _  -> raise (ParseError(r))


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
let rec read inchar char_list =
  try
    read inchar ((input_char inchar)::char_list)
  with End_of_file -> List.rev char_list

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
    let ast = transform st in
    (* let () = print_tree ast; Printf.printf "\n" in *)
    let value = evaluate ast in
    let () = Printf.printf "%i\n" value in
    0
  with ParseError(l) -> (Printf.printf "Parse error at '%s'\n" (implode l); 1);;
