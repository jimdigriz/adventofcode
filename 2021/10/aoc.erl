%%% https://adventofcode.com/2021/day/10

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
-export([part2/0, part2/1]).

%-compile(export_all).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L0 = binary:split(B, <<"\n">>, [global,trim]),
		L = lists:map(fun erlang:binary_to_list/1, L0),
		persistent_term:put({?MODULE,K}, L)
	end, [{example,"input.example"},{input,"input"}]).

part1() ->
	part1(example).
part1(example) ->
	26397 = run1(example);
part1(input) ->
	run1(input).

part2() ->
	part2(example).
part2(example) ->
	288957 = run2(example);
part2(input) ->
	run2(input).

run1(V) ->
	lists:sum(lists:map(fun(R) ->
		S0 = parse([], R),
		if is_list(S0)-> 0; true -> S0 end
	end, persistent_term:get({?MODULE,V}))).

run2(V) ->
	R = lists:usort(lists:map(fun(R) ->
		score2(parse([], R), 0)
	end, lists:filter(fun(R) ->
		is_list(parse([], R))
	end, persistent_term:get({?MODULE,V})))),
	lists:nth(length(R) div 2 + 1, R).

parse([], []) ->
	0;
parse(R0 = [_|_], []) ->
	R0;
parse(R0, [B|R]) when B == $(; B == $[; B == ${; B == $<  ->
	parse([B|R0], R);
parse([B0|R0], [B|R]) when B0 == $( andalso B == $); B0 == $[ andalso B == $]; B0 == ${ andalso B == $}; B0 == $< andalso B == $>  ->
	parse(R0, R);
parse([_B0|_R0], [B|_R]) ->
	score(B).

score(B) when B == $) ->
	3;
score(B) when B == $] ->
	57;
score(B) when B == $} ->
	1197;
score(B) when B == $> ->
	25137.

score2([], A) ->
	A;
score2([B|R], A) when B == $( ->
	score2(R, 5 * A + 1);
score2([B|R], A) when B == $[ ->
	score2(R, 5 * A + 2);
score2([B|R], A) when B == ${ ->
	score2(R, 5 * A + 3);
score2([B|R], A) when B == $< ->
	score2(R, 5 * A + 4).
