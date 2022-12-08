%%% https://adventofcode.com/2022/day/8

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
-export([part2/0, part2/1]).

%-compile(export_all).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = binary:split(B, <<"\n">>, [global,trim]),
		persistent_term:put({?MODULE,K}, parse(L))
	end, [{example,"input.example"},{input,"input"}]).

parse(L) ->
	{length(L),[ H - $0 || B <- L, <<H:8/integer>> <= B ]}.

%%%

part1() ->
	part1(example).
part1(example) ->
	21 = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	{S,M0} = persistent_term:get({?MODULE,V}),
	M = lists:enumerate(0, M0),
	length(lists:filter(fun({I,H}) ->
		X = I rem S,
		Y = I div S,
		LU = [ HH || {II,HH} <- M, II div S < Y, II rem S == X ],
		LL = [ HH || {II,HH} <- M, II div S == Y, II rem S < X ],
		LR = [ HH || {II,HH} <- M, II div S == Y, II rem S > X ],
		LD = [ HH || {II,HH} <- M, II div S > Y, II rem S == X ],
		LUB = lists:dropwhile(fun(HH) -> HH < H end, lists:reverse(LU)),
		LLB = lists:dropwhile(fun(HH) -> HH < H end, lists:reverse(LL)),
		LRB = lists:dropwhile(fun(HH) -> HH < H end, LR),
		LDB = lists:dropwhile(fun(HH) -> HH < H end, LD),
		LUB == [] orelse LLB == [] orelse LRB == [] orelse LDB == []
	end, M)).

%%%

part2() ->
	part2(example).
part2(example) ->
	8 = run2(example);
part2(input) ->
	run2(input).

run2(V) ->
	{S,M0} = persistent_term:get({?MODULE,V}),
	M = lists:enumerate(0, M0),
	lists:max(lists:map(fun({I,H}) ->
		X = I rem S,
		Y = I div S,
		LU = [ HH || {II,HH} <- M, II div S < Y, II rem S == X ],
		LL = [ HH || {II,HH} <- M, II div S == Y, II rem S < X ],
		LR = [ HH || {II,HH} <- M, II div S == Y, II rem S > X ],
		LD = [ HH || {II,HH} <- M, II div S > Y, II rem S == X ],
		{LUV,LUB} = lists:splitwith(fun(HH) -> HH < H end, lists:reverse(LU)),
		{LLV,LLB} = lists:splitwith(fun(HH) -> HH < H end, lists:reverse(LL)),
		{LRV,LRB} = lists:splitwith(fun(HH) -> HH < H end, LR),
		{LDV,LDB} = lists:splitwith(fun(HH) -> HH < H end, LD),
		LUC = length(LUV) + border(LUV, LUB),
		LLC = length(LLV) + border(LLV, LLB),
		LRC = length(LRV) + border(LRV, LRB),
		LDC = length(LDV) + border(LDV, LDB),
		LUC * LLC * LRC * LDC
	end, M)).

border(_V, []) -> 0;
border([], [_|_]) -> 1;
border(V, [H|_]) -> case lists:last(V) < H of true -> 1; false -> 0 end.
