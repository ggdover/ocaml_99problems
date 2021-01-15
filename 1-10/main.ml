
(* Helper functions *)

let go f x =
    match x with
    | Some x -> f x
    | None -> print_endline "Invalid function call"

(*
list = the list to print
f = print function, which differ depending on type of element in list
    e.g. print_string if strings, print_int if ints etc. ....
 *)
let print_list f list =
    let rec iter l =
    match l with
    | [] -> ()
    | h::t -> f h; print_string ","; iter t
    in
    iter list;
    print_endline "";;

print_endline "---- Testing 'print_list' function ---";;
print_list print_int [1;2;3];;
print_list print_string ["a";"b";"c"];;

(* 1 *) 

let rec last list = 
    match list with
    | [] -> None
    | [x] -> Some x
    | _::t -> last t;;

print_endline "---- Testing print results of 'last' ----";;
go print_endline (last ["a";"b";"c"]);;
go print_endline (last []);;

(* 2 *)

let rec last_two list = 
    match list with
    | [] -> None
    | [_] -> None
    | [x;y] -> Some (x,y)
    | _::t -> last_two t;;

(* 3 *)

(* Just some temporary code to test decrementation in recursive call *)
(*
let rec bla x =
    match x with
    | 5 -> 18
    | 0 -> 13
    | _ -> bla x-1;;
*)

let rec at ix list =
    match ix, list with
    | 0, h::_ -> Some h
    | x, _::t -> at (x-1) t
    | _, [] -> None;;

print_endline "---- Testing print results of 'at' ----";;
go print_endline (at 1 ["a";"b";"c"])

(* 4 *)

let count list =
    let rec count_int list n =
        match list with
        | [] -> n
        | _::t -> count_int t n+1
    in
    count_int list 0;;

print_endline "---- Testing print results of 'count' ----";;
print_int (count ["a";"b";"c"]);;
print_endline "";;
print_int (count ["a"]);;
print_endline "";;
print_int (count []);;
print_endline ""

(* 5 *)

(*
FIRST ATTEMPT BEFORE LOOKING AT SOLUTION.
FAILED

let rev_n list =
    let rec rev_int list n = 
        if n == 0 then
            list
        else
            match list with
            | [] -> []
            | h::t -> (rev_int t::h (n-1))
    in
    rev_int list ((count list) / 2)
*)

let rev list =
    let rec rev_int acc lis =
    match lis with
    | [] -> acc
    | h::t -> rev_int (h::acc) t
    in
    rev_int [] list;;

print_endline "---- Testing print results of 'rev' ----";;
print_list print_int (rev [1;2;3]);;
print_list print_string (rev ["a";"b";"c"]);;

(* 6 *)



(* 7 *)

(* 8 *)

(* 9 *)

(* 10 *)