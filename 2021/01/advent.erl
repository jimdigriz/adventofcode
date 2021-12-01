-module(code).

-on_load(init/0).

-export([run/0, run/1]).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = lists:map(fun binary_to_integer/1, binary:split(B, <<"\n">>, [global,trim_all])),
		persistent_term:put({?MODULE,K}, L)
	end, [{input,"input"},{example,"input.example"}]).

run() ->
	run(input).

run(X) when X == example; X == input ->
	run(persistent_term:get({?MODULE,X}));
run(X) when is_list(X) ->
	element(1, lists:foldl(fun
		(XX, {A,L}) when XX > L ->
			{A+1,XX};
		(XX, {A,_L}) ->
			{A,XX}
	end, {0,hd(X)}, tl(X))).
