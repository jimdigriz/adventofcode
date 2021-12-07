%%% https://adventofcode.com/2021/day/6

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
-export([part2/0, part2/1]).

%-compile(export_all).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L0 = binary:split(B, <<"\n">>, [global,trim]),
		L = to_list_of_ints(hd(L0)),
		persistent_term:put({?MODULE,K}, L)
	end, [{example,"input.example"},{input,"input"}]).

to_list_of_ints(B) ->
	lists:map(fun binary_to_integer/1, binary:split(B, [<<",">>,<<" ">>,<<"->">>], [global,trim_all])).

part1() ->
	part1(example).
part1(example) ->
	5934 = run(example, 80);
part1(input) ->
	run(input, 80).

part2() ->
	part2(example).
part2(example) ->
	26984457539 = run(example, 256);
part2(input) ->
	run(input, 256).

run(V, C) ->
	L = persistent_term:get({?MODULE,V}),
	A = lists:foldl(fun(X, AA) ->
		array:set(X, array:get(X, AA) + 1, AA)
	end, empty(), L),
	run2(A, C).
run2(A, 0) ->
	array:foldl(fun(_I, V, AA) -> AA + V end, 0, A);
run2(A0, C) ->
	A = array:foldl(fun
		(I, V, AA0) when I == 0 ->
			array:set(6, V, array:set(8, V, AA0));
		(I, V, AA0) ->
			P = (I + 8) rem 9,
			array:set(P, array:get(P, AA0) + V, AA0)
	end, empty(), A0),
	run2(A, C - 1).

empty() ->
	array:new(9, {default,0}).
