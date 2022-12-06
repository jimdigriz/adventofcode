%%% https://adventofcode.com/2022/day/6

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
-export([part2/0, part2/1]).

%-compile(export_all).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = binary:split(B, <<"\n">>, [global,trim]),
		persistent_term:put({?MODULE,K}, L)
	end, [{example,"input.example"},{input,"input"}]).

%%%

part1() ->
	part1(example).
part1(example) ->
	[7,5,6,10,11] = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	[ marker_packet([], LL) || LL <- L ].

marker_packet(H, _T = <<W, X, Y, Z, _R/binary>>) when W =/= X, W =/= Y, W =/= Z, X =/= Y, X =/= Z, Y =/= Z ->
	length(H) + 4;
marker_packet(H, _T = <<X, R/binary>>) ->
	marker_packet([X|H], R).

%%%

part2() ->
	part2(example).
part2(example) ->
	[19,23,23,29,26] = run2(example);
part2(input) ->
	run2(input).

run2(V) ->
	L = persistent_term:get({?MODULE,V}),
	[ marker_message([], binary_to_list(LL)) || LL <- L ].

marker_message(H, T = [HH|TT]) ->
	{HHH,_} = lists:split(14, T),
	case length(lists:uniq(HHH)) of
		14 ->
			length(H) + 14;
		_ ->
			marker_message([HH|H], TT)
	end.
