open Graphics ;;
open Unix ;;
#load "unix.cma";;
#load "graphics.cma" ;;

let width = 200;;
let height = 200;;

let fps = 5. ;;

type cell = Alive | Dead ;;

type matrice = cell list list ;;

let gride = 10 and size = 20 ;;

 
let rec build (l: int) (c:int) : matrice =
	
let rec col (c: int) : cell list = match c with 
	| 0 -> []
	| _ -> [Dead] @ (col (c-1) ) in

let rec line (l: int) (cl: cell list) : matrice = match l with
	| 0 ->  []
	| _ -> cl ::(line (l-1) cl) in
	
	line l (col c) ;;

let rec div (m: matrice ) (c: int) : matrice  * matrice  =
	let rec aux m c =
	match m with
	| [] -> [] 
	| e::fin -> (if c > 0 then [e] else [] ) @ ( aux fin (c-1) ) 
	in
 let rec aux1 m c =
	match c,m with
	| _ , [] -> []
	| 0 ,e::fin -> e::fin 
	| _ , e::fin -> aux1 fin (c-1) in 
		let x,y = (aux m c) , (aux1 m c) in x,y;;

	
let mid (m: matrice) (n: int)  : matrice * cell list * matrice  =
	let x,y =  div m n in x, (List.hd y) ,(List.tl y);;

let rec insert (l: cell list) (n: int) : cell list  =
	match n,l with
	| a , [] -> []
	| a ,e::fin -> 
		[ (if a = 0 then (if e = Alive then Dead else Alive)
		 else e) ] @ insert fin (n-1)
	
let edit (m: matrice) (cs: (int*int) list) : matrice =
		let rec aux (m: matrice ) (cs: (int*int) list)  : matrice = 
		match cs with
		|(x,y)::r -> let a,b,c = mid m y in
			aux (a @ [ insert b x ] @ c ) r 
		| _ -> m
		 in aux m cs ;;



let rec disp (m: matrice) (x: int) (y:int) : unit =
	match m with
	| [] -> ()
	| e::fin -> print_char '\n' ; print_char '[' ; let rec aux e x y = 
				match e with
				| [] -> ()
				| c ::r -> 
				if c = Alive then
			 	begin print_int 1 ; fill_rect x y size size ;  end 
				else print_int 0
				; aux r (x+size) y in aux e x y;
						; print_char ']' ; disp fin 0 (y+size) 
let construct (u: int) (size: int) : unit =
	let rec itr u =
		match u with
		| 0 -> ()
 		| x -> 
 		draw_segments  [| ( (size * x),0,(size * x),height) |];
 		draw_segments  [| ( 0,(size * x),width,(size * x) ) |];
 		itr (u-1) in itr u

module Time = struct

let sec = Unix.time() ;;
let msec = Unix.gettimeofday() *. 1000. ;;

let wait (ms : float) : unit =
	let r = msec in
	let t = ref r and t_old = ref r in
		while !t < (!t_old +. (100000. /. ms) ) do 
			t := !t +. 0.001 ;
			done ;;
end;;

module Mechanic =
struct

let neighbor_alive (m:  matrice ) (p: int * int) : int = match p,m with
	|(_, []) -> 0
	|(x,y) , e::fin ->  
	let neighbor = ref 0 in
	let a,b  = ref e, ref fin in
		for j = 0 to gride-1 do match !a , !b with | _ , [] -> () | w, h::t ->
					for i = 0 to gride-1 do
						if j >= y-1 && j <= y+1 &&  i >= x-1 && i <= x+1
							then if (List.nth w i) = Alive && (x != i || y != j) then 
								neighbor := !neighbor + 1
					done;
				a := h ; b := t
		 done; !neighbor
		
	
let rec part l m x y = match l with
			| [] -> l
			| e::fin ->
			
		let neighbor = neighbor_alive m (x,y) in 
		let next = 
		if neighbor < 2 && e = Alive then Dead 
		(* under population *)
		else if e =  Alive && (neighbor = 2 || neighbor = 3) then Alive
		(* equil population *)
		else if neighbor > 3 && e = Alive  then Dead
		(* over population *)
		else if e = Dead && neighbor = 3 then Alive 
		(* reproduction *)
		else Dead

		in [next] @ (part fin m (x+1) y) ;;


	let rec all (m: matrice) (s: matrice) (x:int) (y:int) : matrice =
		match m with
		| [] -> m
		| h::t -> (part h s x y ) :: (all t s 0 (y+1) )
end;;




let  start (m: matrice) (g:int) : matrice =
	match g with
	| 0 -> m
	| e -> disp m 0 0 ;	
	let s =  Mechanic.all m m 0 0 in
		 print_char '\n' ; s ;;
		
let setScreen =
  open_graph "";
  resize_window width height;
  set_window_title "Game of life" ;;
  
let exec = 
	let s = wait_next_event [Button_down; Key_pressed] 
     in if s.Graphics.keypressed then
      false else true ;;
      
let main : unit = 

	let s = ref (edit (build gride gride)  
	[(3,5);(4,5);(5,5);(6,5)])  in
	let generation = 20 in
	while exec do
	
		set_color(white);
		fill_rect 0 0 width height;
		set_color(black) ;
	 	construct gride size ;
	 
		s:= start !s generation ;

		Time.wait fps ;
	done;;

(** Exec **)

setScreen ;;
main exec ;;
