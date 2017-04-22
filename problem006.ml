let sum_ints n = 
	let rec aux acc p = if p = n then n+acc else aux (acc+p) (p+1) in aux 0 1
let sum_sq n =
	let rec aux acc p = if p = n then (n*n)+acc else aux (acc+(p*p)) (p+1) in aux 0 1 

let x' = sum_ints 100
let x = x'*x' 
let y = sum_sq 100 
let result = x - y