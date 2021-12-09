%%% https://adventofcode.com/2021/day/8

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).

%-compile(export_all).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L0 = binary:split(B, <<"\n">>, [global,trim]),
		L = lists:map(fun erlang:binary_to_list/1, L0),
		persistent_term:put({?MODULE,K}, L)
	end, [{example,"input.example"},{input,"input"}]).

parse(V) ->
	L = persistent_term:get({?MODULE,V}),
	AM = lists:map(fun(X0) ->
		X = [infinity|lists:map(fun(XX) -> XX - $0 end, X0)],
		array:from_list(X, infinity)
	end, L),
	AINF = array:new([{default,infinity}]),
	array:from_list([AINF|AM], AINF).

part1() ->
	part1(example).
part1(example) ->
	15 = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	A = parse(V),
	lists:foldl(fun(Y, S) ->
		AY = array:get(Y, A),
		lists:foldl(fun(X, SS) ->
			U = array:get(X, array:get(Y - 1, A)),
			D = array:get(X, array:get(Y + 1, A)),
			L = array:get(X - 1, AY),
			R = array:get(X + 1, AY),
			H = array:get(X, AY),
			if
				H < U, H < D, H < L, H < R ->
					SS + 1 + H;
				true ->
					SS
			end
		end, S, lists:seq(1, array:size(AY) - 1))
	end, 0, lists:seq(1, array:size(A) - 1)).
