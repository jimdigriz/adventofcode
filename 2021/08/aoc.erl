%%% https://adventofcode.com/2021/day/8

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).

%-compile(export_all).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = parse(binary:split(B, <<"\n">>, [global,trim])),
		persistent_term:put({?MODULE,K}, L)
	end, [{example,"input.example"},{input,"input"}]).

parse(R) ->
	parse(R, []).
parse([], A) ->
	lists:reverse(A);
parse([L0|R], A) ->
	{U0,[<<"|">>|N0]} = lists:split(10, binary:split(L0, <<" ">>, [global,trim])),
	U = lists:map(fun(X) -> ordsets:from_list(binary_to_list(X)) end, U0),
	N = lists:map(fun(X) -> ordsets:from_list(binary_to_list(X)) end, N0),
	parse(R, [{U,N}|A]).

part1() ->
	part1(example).
part1(example) ->
	26 = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	lists:foldl(fun({_U,N}, A) ->
		A + length(lists:filter(fun
			(X) when length(X) == 2; length(X) == 4; length(X) == 3; length(X) == 7 ->
				true;
			(_X) ->
				false
		end, N))
	end, 0, L).
