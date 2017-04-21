let make_int_list a b =
	let rec aux acc i = if i < (min a b) then acc else aux (i::acc) (i-1)
in if a < b then  aux [] b else List.rev(aux [] a)

let sum l =
	let rec aux acc = function
	| [] -> acc
	| x::xs -> aux (x+acc) xs
	in
	aux 0 l 

let fibonacci n =
	let rec aux a b i =
		if i=1 then b else aux b (a+b) (i-1) in 
	if n = 0 || n = 1 then 1 else aux 1 1 n

let fib n = 
	let r = lazy (fibonacci n) in
	Lazy.force r

let f max_val = 
	let rec iterate_fib acc i =
		if (fib i) >= max_val then acc else iterate_fib ((fib i)::acc) (i+1)
	in 
	List.rev (iterate_fib [] 0)

let xs = f 4000000
let s = sum ( List.filter (fun x -> x mod 2 = 0) xs )
;;
