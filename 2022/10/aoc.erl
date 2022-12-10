%%% https://adventofcode.com/2022/day/10

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
%-export([part2/0, part2/1]).

%-compile(export_all).

-record(state, {
	cycle	= 0	:: non_neg_integer(),
	regx	= 1	:: integer(),
	inst		:: list(inst()),
	hist	= []	:: list(integer())
}).
-type inst() :: noop | {addx,integer()}.

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = binary:split(B, <<"\n">>, [global,trim]),
		persistent_term:put({?MODULE,K}, parse(L))
	end, [{example,"input.example"},{input,"input"}]).

parse(L) ->
	lists:map(fun
		(<<"noop">>) -> noop;
		(<<"addx ", V/binary>>) -> {addx,binary_to_integer(V)}
	end, L).

%%%

part1() ->
	part1(example).
part1(example) ->
	T = [{20,21},{60,19},{100,18},{140,21},{180,16},{220,18}] = run1(example),
	13140 = lists:foldl(fun({C,X}, A) -> A + (C * X) end, 0, T);
part1(input) ->
	T = run1(input),
	lists:foldl(fun({C,X}, A) -> A + (C * X) end, 0, T).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	#state{ hist = HR } = lists:foldl(fun
		(noop, SS = #state{ cycle = CC, regx = XX, hist = HH }) ->
			SS#state{ cycle = CC + 1, hist = [XX|HH] };
		({addx,VV}, SS = #state{ cycle = CC, regx = XX, hist = HH }) ->
			SS#state{ cycle = CC + 2, hist = [XX,XX|HH], regx = XX+VV }
	end, #state{}, L),
	[ {C,X} || {C,X} <- lists:enumerate(lists:reverse(HR)), (C - 20) rem 40 == 0 ].
