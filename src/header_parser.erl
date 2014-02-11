-module (header_parser).
-export ([parse/1]).
parse([ H | T ]) ->
	[parse(H) | parse(T)];
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

trim_binary_string(<<String/bits>>) ->
	binary:list_to_bin(string:strip(binary:bin_to_list(String))).