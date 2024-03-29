%%% https://adventofcode.com/2022/day/3

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

priority(X) when X >= $A, X =< $Z -> 27 + X - $A;
priority(X) when X >= $a, X =< $z -> 1 + X - $a.

%%%

part1() ->
	part1(example).
part1(example) ->
	157 = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	S = lists:map(fun(LL) ->
		{LL1,LL2} = lists:split(byte_size(LL) div 2, binary_to_list(LL)),
		{sets:from_list(LL1),sets:from_list(LL2)}
	end, L),
	lists:sum([ priority(hd(sets:to_list(sets:intersection(S1, S2)))) || {S1,S2} <- S ]).

%%%

part2() ->
	part2(example).
part2(example) ->
	70 = run2(example);
part2(input) ->
	run2(input).

run2(V) ->
	L = persistent_term:get({?MODULE,V}),
	S = [ sets:from_list(lists:usort(binary_to_list(LL))) || LL <- L ],
	do_run2(S, []).

do_run2([], R) ->
	lists:sum(R);
do_run2([S1,S2,S3|S], R) ->
	P = priority(hd(sets:to_list(sets:intersection([S1, S2, S3])))),
	do_run2(S, [P|R]).
