let make_list a b =
	let rec aux acc i = if i < (min a b) then acc else aux (i::acc) (i-1)
in if a < b then  aux [] b else List.rev(aux [] a)

let sum l =
	let rec aux acc = function
	| [] -> acc
	| x::xs -> aux (x+acc) xs
	in
	aux 0 l 


let finder listofints nmax = 
	let r = make_list 1 (nmax-1) in
	List.filter (fun y -> List.exists (fun x -> y mod x = 0) listofints) r

let x = [3;5] 
let n = 1000 
let result = sum(finder x n)
