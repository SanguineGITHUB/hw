type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string


let rec checkMetro : metro -> bool = fun m ->
	match m with
	| STATION id -> false
	| AREA (n, _m) -> 
		match _m with
		| STATION _id -> if _id = n then true else false
		| _ -> checkMetro(_m)


let _ =
let print_bool x = 
	print_endline (string_of_bool x) in 
		print_bool(checkMetro( AREA("a", STATION "a") ));
		print_bool(checkMetro( AREA("a", AREA("a", STATION "a"))));
		print_bool(checkMetro( AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))));
		print_bool(checkMetro( AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))));
		print_bool(checkMetro( AREA("a", STATION "b")));
		print_bool(checkMetro( AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))));
		print_bool(checkMetro( AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))));

