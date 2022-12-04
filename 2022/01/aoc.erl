%%% https://adventofcode.com/2022/day/01

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

parse(L) ->
	parse(L, [0]).
parse([], R) ->
	lists:enumerate(lists:reverse(R));
parse([<<>>|L], R) ->
	parse(L, [0|R]);
parse([V|L], [C|R]) ->
	parse(L, [C + binary_to_integer(V)|R]).

%%%

part1() ->
	part1(example).
part1(example) ->
	{4,24000} = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	R = parse(L),
	do_run1(R).

do_run1(R) ->
	lists:last(lists:keysort(2, R)).

%%%

part2() ->
	part2(example).
part2(example) ->
	45000 = run2(example);
part2(input) ->
	run2(input).

run2(V) ->
	L = persistent_term:get({?MODULE,V}),
	R = parse(L),
	do_run2(R).

do_run2(R) ->
	{_,[A,B,C|_]} = lists:unzip(lists:reverse(lists:keysort(2, R))),
	A + B + C.
