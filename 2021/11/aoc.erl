%%% https://adventofcode.com/2021/day/11

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).

%-compile(export_all).

-define(MIN_INT, -576460752303423488).

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
	1656 = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	A = array:fix(array:from_list(lists:map(fun(X) -> X - $0 end, lists:flatten(L)))),
	run1(A, 4, 0).

run1(_A, 0, C) ->
	C;
run1(A0, L, C) ->
	A = array:map(fun(_I, V) -> V + 1 end, A0),
	run12(A, L, C).
run12(A0, L, C0) ->
io:format("~b ~b ~p~n", [L, C0, A0]),
	{A,C} = array:foldl(fun
		(I, V, {AA0,CC}) when V > 9 ->
			N = [I - 11, I - 10, I - 9, I - 1, I + 1, I + 9, I + 10, I + 11],
			AA1 = array:set(I, 0, AA0),
			AA = lists:foldl(fun
				(NN, AAA) when NN >= 0, NN < 100 ->
					VV = array:get(NN, AAA),
					if VV == 0 -> AAA; true -> array:set(NN, VV + 1, AAA) end;
				(_NN, AAA) ->
					AAA
			end, AA1, N),
			{AA,CC + 1};
		(_I, _V, {AA,CC}) ->
			{AA,CC}
	end, {A0,C0}, A0),
	if
		C == C0 ->
			run1(A, L - 1, C);
		true ->
			run12(A, L, C)
	end.
