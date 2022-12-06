%%% https://adventofcode.com/2022/day/6

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
%-export([part2/0, part2/1]).

%-compile(export_all).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = binary:split(B, <<"\n">>, [global,trim]),
		persistent_term:put({?MODULE,K}, L)
	end, [{example,"input.example"},{input,"input"}]).

%%%

part1() ->
	part1(example).
part1(example) ->
	[7,5,6,10,11] = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	[ marker([], LL) || LL <- L ].

marker(H, _T = <<W, X, Y, Z, _R/binary>>) when W =/= X, W =/= Y, W =/= Z, X =/= Y, X =/= Z, Y =/= Z ->
	length(H) + 4;
marker(H, _T = <<X, R/binary>>) ->
	marker([X|H], R).
