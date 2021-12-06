%%% https://adventofcode.com/2021/day/6

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).

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
	5934 = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	run1(L, 80).
run1(L, 0) ->
	length(L);
run1(L0, C) ->
	L = lists:foldl(fun
		(0, A) ->
			[6,8|A];
		(I, A) ->
			[I-1|A]
	end, [], L0),
	run1(L, C - 1).
