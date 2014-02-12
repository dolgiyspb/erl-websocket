-module (header_parser).
-export ([parse_header/1, get_header/2]).

parse_header(H) ->
	parse(binary:split(H, <<"\r\n">>, [global])).

parse([ H | T ]) ->
	lists:flatten([parse(H) | parse(T)]);
parse([]) ->
	[];
parse(<<"GET ", Rest/bits>>) ->
	parse(binary:split(Rest, <<" HTTP/">>));
parse(Path = <<"/", _/bits>>) ->
	{ path, Path};
parse(<< Major/integer, ".", Minor/integer >>) ->
	CharToInt = fun(C) -> {Int, _} = string:to_integer([C]), Int end,
	{version, { major, CharToInt(Major) }, { minor, CharToInt(Minor) }};
parse(<<Header/bits>>) ->	
	[ Key, Value ] = binary:split(Header, <<":">>),
	parse(Key, Value).

parse(Key = <<"Accept">>, Values = <<_/bits>>) ->
	parse(Key, binary:split(Values, <<",">>, [global]));	

parse(Key, Value) ->
	{ trim_binary_string(Key), trim_binary_string(Value) }. 

trim_binary_string([H|T]) ->
	lists:filter(fun is_bitstring/1 ,[trim_binary_string(H) | trim_binary_string(T)]);
trim_binary_string([]) ->
	[];
trim_binary_string(<<"">>) ->
	null;	
trim_binary_string(<<String/bits>>) ->
	binary:list_to_bin(string:strip(binary:bin_to_list(String))).

get_header(<<Header/bits>>, Headers) ->
	lists:keyfind(Header, 1, Headers).
