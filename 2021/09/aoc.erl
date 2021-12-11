%%% https://adventofcode.com/2021/day/9

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

part2() ->
	part2(example).
part2(example) ->
	1134 = run2(example);
part2(input) ->
	run2(input).

basins(A) ->
	lists:foldl(fun(Y, B) ->
		AY = array:get(Y, A),
		lists:foldl(fun(X, BB) ->
			N = array:get(X, array:get(Y - 1, A)),
			S = array:get(X, array:get(Y + 1, A)),
			W = array:get(X - 1, AY),
			E = array:get(X + 1, AY),
			H = array:get(X, AY),
			if
				H < N, H < S, H < W, H < E ->
					[{Y,X}|BB];
				true ->
					BB
			end
		end, B, lists:seq(1, array:size(AY) - 1))
	end, [], lists:seq(1, array:size(A) - 1)).

run1(V) ->
	A = parse(V),
	B = basins(A),
        lists:foldl(fun({Y,X}, S) ->
		S + 1 + array:get(X, array:get(Y, A))
	end, 0, B).

run2(V) ->
	A = parse(V),
	B = basins(A),
	run2(A, B, []).
run2(_A, [], R0) ->
	lists:foldl(fun({_B,F}, A) ->
		A * ordsets:size(F)
	end, 1, lists:sublist(lists:sort(fun({_AB,BF}, {_BB,AF}) ->
		ordsets:size(BF) > ordsets:size(AF)
	end, R0), 3));
run2(A, [B|BR], R) ->
	I = ordsets:from_list([B]),
	F = run22(A, I, I),
	run2(A, BR, [{B,F}|R]).
run22(_A, F, []) ->
	F;
run22(A, F0, P0) ->
	{F,P} = ordsets:fold(fun(_B = {BY,BX}, AA) ->
		H = array:get(BX, array:get(BY, A)),
		lists:foldl(fun(BB = {CY,CX}, AAA = {FF0,PP0}) ->
			C = array:get(CX, array:get(CY, A)),
			I = ordsets:is_element(BB, ordsets:union(FF0, PP0)),
			if
				not I, C > H, C < 9 ->
					FF = ordsets:add_element(BB, FF0),
					PP = ordsets:add_element(BB, PP0),
					{FF,PP};
				true ->
					AAA
			end
		end, AA, [{BY-1,BX},{BY+1,BX},{BY,BX-1},{BY,BX+1}])
	end, {F0,ordsets:new()}, P0),
	run22(A, F, P).
