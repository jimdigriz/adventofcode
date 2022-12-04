%%% https://adventofcode.com/2022/day/02

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).

%-compile(export_all).

-define(LOSE, 0).
-define(DRAW, 3).
-define(WIN, 6).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L0 = binary:split(B, <<"\n">>, [global,trim]),
		L = lists:map(fun(<<X, " ", Y>>) -> {map(X),map(Y)} end, L0),
		persistent_term:put({?MODULE,K}, L)
	end, [{example,"input.example"},{input,"input"}]).

map(X) when X == $A; X == $X -> rock;
map(X) when X == $B; X == $Y -> paper;
map(X) when X == $C; X == $Z -> scissors.

score(X, Y) when X == Y -> item(Y) + ?DRAW;
score(rock, Y = paper) -> item(Y) + ?WIN;
score(rock, Y = scissors) -> item(Y) + ?LOSE;
score(paper, Y = rock) -> item(Y) + ?LOSE;
score(paper, Y = scissors) -> item(Y) + ?WIN;
score(scissors, Y = rock) -> item(Y) + ?WIN;
score(scissors, Y = paper) -> item(Y) + ?LOSE.

item(rock) -> 1;
item(paper) -> 2;
item(scissors) -> 3.

%%%

part1() ->
	part1(example).
part1(example) ->
	15 = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	do_run1(L).

do_run1(L) ->
	lists:foldl(fun({X,Y}, A) -> score(X, Y) + A end, 0, L).

%%%
