%%% https://adventofcode.com/2022/day/8

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
%-export([part2/0, part2/1]).

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
		LL = [ HH || {II,HH} <- M, II div S == Y, II rem S < X ],
		LR = [ HH || {II,HH} <- M, II div S == Y, II rem S > X ],
		LU = [ HH || {II,HH} <- M, II div S < Y, II rem S == X ],
		LD = [ HH || {II,HH} <- M, II div S > Y, II rem S == X ],
		LLB = lists:dropwhile(fun(HH) -> HH < H end, LL),
		LRB = lists:dropwhile(fun(HH) -> HH < H end, LR),
		LDB = lists:dropwhile(fun(HH) -> HH < H end, LD),
		LUB = lists:dropwhile(fun(HH) -> HH < H end, LU),
		LLB == [] orelse LRB == [] orelse LDB == [] orelse LUB == []
	end, M)).
