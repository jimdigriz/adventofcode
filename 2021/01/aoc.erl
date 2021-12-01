-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
-export([part2/0, part2/1]).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = lists:map(fun binary_to_integer/1, binary:split(B, <<"\n">>, [global,trim_all])),
		persistent_term:put({?MODULE,K}, L)
	end, [{example,"input.example"},{input,"input"}]).

part1() ->
	part1(example).

part1(example) ->
	7 = run(persistent_term:get({?MODULE,example}));
part1(input) ->
	run(persistent_term:get({?MODULE,input})).

part2() ->
	part2(example).

part2(example) ->
	5 = run(persistent_term:get({?MODULE,example}), 3);
part2(input) ->
	run(persistent_term:get({?MODULE,input}), 3).

run(L) ->
	run(L, 1).

run(L, W) ->
	run(L, W, []).

run(L = [_X|R], W, A) when length(L) >= W ->
	AN = lists:sum(element(1, lists:split(W, L))),
	run(R, W, [AN|A]);
run(_L, _W, A) ->
	run2(lists:reverse(A)).

run2([I|R]) ->
	element(1, lists:foldl(fun
		(XX,{A,L}) when XX > L ->
			{A+1,XX};
		(XX,{A,_L}) ->
			{A,XX}
	end, {0,I}, R)).
