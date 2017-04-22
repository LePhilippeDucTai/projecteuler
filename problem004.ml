let is_palindrome n = 
	let x = string_of_int n in 
	let reverse s = 
		let p = String.length x in 
		String.init p (fun i -> x.[p-i-1])
	in
	reverse x = x 

let make_list a b =
	let rec aux acc i = if i < (min a b) then acc else aux (i::acc) (i-1)
in if a < b then  aux [] b else List.rev(aux [] a)

let xs1 = make_list 999 100
let xs2 = make_list 990 100
let product = List.fold_left (fun y x  -> max x y) 0
	(List.concat(List.map (fun x -> (List.filter is_palindrome (List.map (fun y -> x*y) xs1))) xs2)) ;;


