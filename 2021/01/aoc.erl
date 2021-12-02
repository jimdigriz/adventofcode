%%% https://adventofcode.com/2021/day/1

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
-export([part2/0, part2/1]).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = lists:map(fun binary_to_integer/1, binary:split(B, <<"\n">>, [global,trim_all])),
		persistent_term:put({?MODULE,K}, L)
	end, [{example,"input.example"},{input,"input"}]).

part1() ->
	part1(example).
part1(example) ->
	7 = run(example);
part1(input) ->
	run(input).

part2() ->
	part2(example).
part2(example) ->
	5 = run(example, 3);
part2(input) ->
	run(input, 3).

run(V) ->
	run(V, 1).
run(V, W) ->
	L = persistent_term:get({?MODULE,V}),
	{H,T} = lists:split(W, L),
	run(H, T, []).
run(H = [_HH|HR], _T = [TH|TR], A) ->
	S = lists:sum(H),
	run(HR ++ [TH], TR, [S|A]);
run(H, _T = [], A) ->
	S = lists:sum(H),
	run2(lists:reverse([S|A])).

run2([I|R]) ->
	element(2, lists:foldl(fun
		(X,{L,A}) when X > L ->
			{X,A+1};
		(X,{_L,A}) ->
			{X,A}
	end, {I,0}, R)).
