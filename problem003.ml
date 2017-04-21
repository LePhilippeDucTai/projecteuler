let make_list a b =
	let rec aux acc i = if i < (min a b) then acc else aux (i::acc) (i-1)
in if a < b then  aux [] b else List.rev(aux [] a)

let is_prime n =
	match n with
	| 0 | 1 -> false 
	| 2 | 3  -> true 
	| _ ->
		let p = (int_of_float (sqrt(float_of_int n)))+1 in 
		let rec test x =
			if x = p then true else
				if n mod x = 0 then false else test (x+1) 
			in test 2 

let list_primes n = List.filter is_prime (make_list 1 n)

let x = 600851475143L
let p = int_of_float (sqrt (Int64.to_float 600851475143L))
let l = list_primes p

let find_largest_factor n =
	let p = int_of_float (sqrt (Int64.to_float n)) in
	let ls = list_primes p in
	let ls = List.map Int64.of_int ls in
	let rec aux acc newint primes_list=
		match primes_list with
		| [] -> acc
		| x::xs -> 
			if (Int64.rem newint x) = Int64.zero then 
				aux (x::acc) (Int64.div newint x) primes_list
			else aux acc newint xs 
	in 
	aux [] n ls	

let result = find_largest_factor 600851475143L ;;




