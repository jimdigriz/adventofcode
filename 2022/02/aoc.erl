%%% https://adventofcode.com/2022/day/2

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
-export([part2/0, part2/1]).

%-compile(export_all).

-define(LOSE, 0).
-define(DRAW, 3).
-define(WIN, 6).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L0 = binary:split(B, <<"\n">>, [global,trim]),
		L = [ {map(X),map(Y)} || <<X, " ", Y>> <- L0 ],
		persistent_term:put({?MODULE,K}, L)
	end, [{example,"input.example"},{input,"input"}]).

map(X) when X >= $A, X =< $C -> X - $A;
map(X) when X >= $X, X =< $Z -> X - $X.

score(X, Y) when X == Y -> (Y + 1) + ?DRAW;
score(X, Y) when X == (3 + Y - 1) rem 3 -> (Y + 1) + ?WIN;
score(X, Y) when X == (Y + 1) rem 3 -> (Y + 1) + ?LOSE.

%%%

part1() ->
	part1(example).
part1(example) ->
	15 = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	lists:sum([ score(X, Y) || {X,Y} <- L ]).

%%%

part2() ->
	part2(example).
part2(example) ->
	12 = run2(example);
part2(input) ->
	run2(input).

run2(V) ->
	L = persistent_term:get({?MODULE,V}),
	lists:sum([ score(X, play(X, Y)) || {X,Y} <- L ]).

play(X, _Y = 0) -> (3 + X - 1) rem 3;
play(X, _Y = 1) -> X;
play(X, _Y = 2) -> (X + 1) rem 3.
