let rec pgcd a b = 
	if b = 0 then a else pgcd b (a mod b) 

let ppcm a b = a*b/(pgcd a b)

let find n = 
	let rec aux acc i = if i=2  then acc else aux (ppcm i acc) (i-1) 
in aux 1 n

let result = find 20 
;;