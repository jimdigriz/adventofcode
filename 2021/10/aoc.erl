%%% https://adventofcode.com/2021/day/10

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).

-compile(export_all).

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

run1(V) ->
	lists:sum(lists:map(fun(R) ->
		parse([], R)
	end, persistent_term:get({?MODULE,V}))).

parse([_|_], []) ->
	0;
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
