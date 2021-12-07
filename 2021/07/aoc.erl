%%% https://adventofcode.com/2021/day/7

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
%-export([part2/0, part2/1]).

-compile(export_all).

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
	{2,37} = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	{Min,Max} = lists:foldl(fun(X, {LL,HH}) ->
		{min(X,LL),max(X,HH)}
	end, {0,0}, L),
	run1(L, Min, Max, undefined, infinity, Min).

run1(_L, _Min, Max, BestPosition, BestFuel, Position) when Position > Max ->
	{BestPosition,BestFuel};
run1(L, Min, Max, BestPosition0, BestFuel0, Position) ->
	{BestPosition, BestFuel} = case fuel(L, Position) of
		Fuel when Fuel < BestFuel0 ->
			{Position,Fuel};
		_Fuel ->
			{BestPosition0,BestFuel0}
	end,
	run1(L, Min, Max, BestPosition, BestFuel, Position + 1).

fuel(L, Position) ->
	lists:foldl(fun(X, A) -> A + abs(X - Position) end, 0, L).
