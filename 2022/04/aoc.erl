%%% https://adventofcode.com/2022/day/4

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
-export([part2/0, part2/1]).

%-compile(export_all).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = binary:split(B, <<"\n">>, [global,trim]),
		persistent_term:put({?MODULE,K}, L)
	end, [{example,"input.example"},{input,"input"}]).

parse(L) when is_list(L) ->
	lists:map(fun parse/1, L);
parse(L) ->
	[A,B] = binary:split(L, <<",">>),
	[A1,A2] = lists:map(fun binary_to_integer/1, binary:split(A, <<"-">>)),
	[B1,B2] = lists:map(fun binary_to_integer/1, binary:split(B, <<"-">>)),
	{{A1,A2},{B1,B2}}.

%%%

part1() ->
	part1(example).
part1(example) ->
	2 = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	{S,_} = lists:partition(fun({{A1,A2},{B1,B2}}) ->
		(A1 >= B1 andalso A2 =< B2) orelse (B1 >= A1 andalso B2 =< A2)
	end, parse(L)),
	length(S).

%%%

part2() ->
	part2(example).
part2(example) ->
	4 = run2(example);
part2(input) ->
	run2(input).

run2(V) ->
	L0 = persistent_term:get({?MODULE,V}),
	L = lists:map(fun({{A1,A2},{B1,B2}}) ->
		{sets:from_list(lists:seq(A1, A2)), sets:from_list(lists:seq(B1, B2))}
	end, parse(L0)),
	{S,_} = lists:partition(fun({A,B}) ->
		sets:size(sets:intersection(A, B)) > 0
	end, L),
	length(S).
