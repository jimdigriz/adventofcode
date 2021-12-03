%%% https://adventofcode.com/2021/day/3

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = binary:split(B, <<"\n">>, [global,trim_all]),
		persistent_term:put({?MODULE,K}, L)
	end, [{example,"input.example"},{input,"input"}]).

part1() ->
	part1(example).
part1(example) ->
	GE = {G = 22,E = 9} = run(example),
	{GE,G*E};
part1(input) ->
	GE = {G,E} = run(input),
	{GE,G*E}.

run(V) ->
	L0 = persistent_term:get({?MODULE,V}),
	L = lists:map(fun binary_to_list/1, L0),
	A = [ 0 || _ <- lists:seq(1, length(hd(L))) ],
	run(L, A, 0).
run([H|R], A0, C) ->
	A = lists:map(fun({X,Y}) -> (X-$0) + Y end, lists:zip(H, A0)),
	run(R, A, C + 1);
run([], A, C) ->
	erlang:delete_element(3, lists:foldr(fun
		(X, {G,E,S}) when X > (C div 2) ->
			{G+S,E,2*S};
		(_X, {G,E,S}) ->
			{G,E+S,2*S}
	end, {0,0,1}, A)).
