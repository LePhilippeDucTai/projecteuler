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

let largestnthprime n =
	let rec iterate lastprime current_n nprimes =
		if nprimes > n then lastprime else
			if is_prime current_n then iterate current_n (current_n+1) (nprimes+1)
		else iterate lastprime (current_n +1) nprimes
	in iterate 2 2 1
;;