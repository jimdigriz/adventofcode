%%% https://adventofcode.com/2021/day/5

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
-export([part2/0, part2/1]).

%-compile(export_all).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = parse(binary:split(B, <<"\n">>, [global,trim])),
		persistent_term:put({?MODULE,K}, L)
	end, [{example,"input.example"},{input,"input"}]).

parse(L) ->
	parse(L, []).
parse([R|L], A) ->
	[X1,Y1,X2,Y2] = to_list_of_ints(R),
	parse(L, [{{X1,Y1},{X2,Y2}}|A]);
parse([], A) ->
	lists:reverse(A).

to_list_of_ints(B) ->
	lists:map(fun binary_to_integer/1, binary:split(B, [<<",">>,<<" ">>,<<"->">>], [global,trim_all])).

part1() ->
	part1(example).
part1(example) ->
	5 = run1(example);
part1(input) ->
	run1(input).

part2() ->
	part2(example).
part2(example) ->
	12 = run2(example);
part2(input) ->
	run2(input).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	run1(L, dict:new()).
run1([{{X1,Y1},{X2,Y2}}|L], D0) when X1 == X2 ->
	D = lists:foldl(fun({X,Y}, DD) ->
		dict:update_counter({X,Y}, 1, DD)
	end, D0, lists:map(fun(Y) ->
		{X1,Y}
	end, lists:seq(Y1, Y2, if Y1 < Y2 -> 1; true -> -1 end))),
	run1(L, D);
run1([{{X1,Y1},{X2,Y2}}|L], D0) when Y1 == Y2 ->
	D = lists:foldl(fun({X,Y}, DD) ->
		dict:update_counter({X,Y}, 1, DD)
	end, D0, lists:map(fun(X) ->
		{X,Y1}
	end, lists:seq(X1, X2, if X1 < X2 -> 1; true -> -1 end))),
	run1(L, D);
run1([{{_X1,_Y1},{_X2,_Y2}}|L], D) ->
	run1(L, D);
run1([], D) ->
	dict:size(dict:filter(fun(_K, V) -> V > 1 end, D)).

run2(V) ->
	L = persistent_term:get({?MODULE,V}),
	run2(L, dict:new()).
run2([{{X1,Y1},{X2,Y2}}|L], D0) ->
	XL0 = lists:seq(X1, X2, if X1 < X2 -> 1; true -> -1 end),
	YL0 = lists:seq(Y1, Y2, if Y1 < Y2 -> 1; true -> -1 end),
	XL = if X1 == X2 -> [X1 || _ <- YL0]; true -> XL0 end,
	YL = if Y1 == Y2 -> [Y1 || _ <- XL0]; true -> YL0 end,
	D = lists:foldl(fun({X,Y}, DD) ->
		dict:update_counter({X,Y}, 1, DD)
	end, D0, lists:zip(XL,YL)),
	run2(L, D);
run2([], D) ->
	dict:size(dict:filter(fun(_K, V) -> V > 1 end, D)).
