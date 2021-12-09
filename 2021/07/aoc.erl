%%% https://adventofcode.com/2021/day/7

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
-export([part2/0, part2/1]).

%-compile(export_all).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L0 = binary:split(B, <<"\n">>, [global,trim]),
		L = to_list_of_ints(hd(L0)),
		persistent_term:put({?MODULE,K}, L)
	end, [{example,"input.example"},{input,"input"}]).

to_list_of_ints(B) ->
	lists:map(fun binary_to_integer/1, binary:split(B, [<<",">>,<<" ">>,<<"->">>], [global,trim_all])).

part1() ->
	part1(example).
part1(example) ->
	{2,37} = run(example, fun fuel1/2);
part1(input) ->
	run(input, fun fuel1/2).

part2() ->
	part2(example).
part2(example) ->
	{5,168} = run(example, fun fuel2/2);
part2(input) ->
	run(input, fun fuel2/2).

run(V, FuelFun) ->
	L = persistent_term:get({?MODULE,V}),
	{Min,Max} = lists:foldl(fun(X, {LL,HH}) ->
		{min(X,LL),max(X,HH)}
	end, {0,0}, L),
	run(L, FuelFun, Min, Max, undefined, infinity, Min).

run(_L, _FuelFun, _Min, Max, BestPosition, BestFuel, Position) when Position > Max ->
	{BestPosition,BestFuel};
run(L, FuelFun, Min, Max, BestPosition0, BestFuel0, Position) ->
	{BestPosition, BestFuel} = case FuelFun(L, Position) of
		Fuel when Fuel < BestFuel0 ->
			{Position,Fuel};
		_Fuel ->
			{BestPosition0,BestFuel0}
	end,
	run(L, FuelFun, Min, Max, BestPosition, BestFuel, Position + 1).

fuel1(L, Position) ->
	lists:foldl(fun(X, A) -> A + abs(X - Position) end, 0, L).

fuel2(L, Position) ->
	lists:foldl(fun(X, A) ->
		O = abs(X - Position),
		A + ((O*(O+1)) div 2)	% https://en.wikipedia.org/wiki/1_%2B_2_%2B_3_%2B_4_%2B_%E2%8B%AF
	end, 0, L).
