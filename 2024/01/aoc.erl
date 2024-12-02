%%% https://adventofcode.com/2024/day/01

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
%-export([part2/0, part2/1]).

-compile(export_all).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = binary:split(B, <<"\n">>, [global,trim]),
		persistent_term:put({?MODULE,K}, parse(L))
	end, [{example,"input.example"},{input,"input"}]).

parse(L) ->
	parse(L, [], []).
parse([], A, B) ->
	{lists:reverse(A), lists:reverse(B)};
parse([R|L], A0, B0) ->
	[AA0,BB0] = binary:split(R, <<" ">>, [global,trim_all]),
	AA = binary_to_integer(AA0),
	BB = binary_to_integer(BB0),
	parse(L, [AA|A0], [BB|B0]).

%%%

part1() ->
	part1(example).
part1(example) ->
	L = [2,1,0,1,2,5] = run1(example),
	lists:sum(L);
part1(input) ->
	lists:sum(run1(input)).

run1(V) ->
	{A0,B0} = persistent_term:get({?MODULE,V}),
	A = lists:sort(A0),
	B = lists:sort(B0),
	lists:map(fun({AA, BB}) ->
		abs(AA - BB)
	end,  lists:zip(A, B)).

%%%
