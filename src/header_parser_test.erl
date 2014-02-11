-module (header_parser_test).
-include_lib("/usr/lib/erlang/lib/eunit-2.2.2/include/eunit.hrl"). 
parse_version_test() ->
	{ version, {major, 1}, { minor, 0} } = header_parser:parse(<<"1.0">>).
parse_path_test() -> 
	{path, <<"/path">>} = header_parser:parse(<<"/path">>).
parse_GET_header_test() ->
	[{path,<<"/path">>},{ version, {major, 1}, { minor, 0} }] = header_parser:parse(<<"GET /path HTTP/1.0">>).
parse_simple_header_test() ->
	{ <<"Header">>, <<"test">> } = header_parser:parse(<<"Header: test">>).