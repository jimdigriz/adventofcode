%%% https://adventofcode.com/2021/day/4

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
-export([part2/0, part2/1]).

%-compile(export_all).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		{NN,BB} = parse(binary:split(B, <<"\n">>, [global,trim])),
		persistent_term:put({?MODULE,K,numbers}, NN),
		persistent_term:put({?MODULE,K,boards}, BB)
	end, [{example,"input.example"},{input,"input"}]).

parse([N0|R]) ->
	N = to_list_of_ints(N0),
	parse(R, {N,[]}).
parse([<<>>|R0], {N,B}) ->
	{BB0,R} = lists:split(5, R0),
	BB = lists:map(fun to_list_of_ints/1, BB0),
	parse(R, {N,[BB|B]});
parse([], {N,B}) ->
	{N,lists:reverse(B)}.

to_list_of_ints(B) ->
	lists:map(fun binary_to_integer/1, binary:split(B, [<<",">>,<<" ">>], [global,trim_all])).

part1() ->
	part1(example).
part1(example) ->
	SN = {S = 188,N = 24} = run1(example),
	{SN,S*N};
part1(input) ->
	SN = {S,N} = run1(input),
	{SN,S*N}.

part2() ->
	part2(example).
part2(example) ->
	SN = {S = 148,N = 13} = run2(example),
	{SN,S*N};
part2(input) ->
	SN = {S,N} = run2(input),
	{SN,S*N}.

to_board_sets(B) ->
	lists:map(fun sets:from_list/1, B ++ board_rotate(B)).

board_rotate(B) ->
	board_rotate(B, []).
board_rotate([[]|_], BF) ->
	lists:reverse(BF);
board_rotate(B0, BF0) ->
	{B,BF} = lists:mapfoldr(fun([X|XR], A) ->
		{XR,[X|A]}
	end, [], B0),
	board_rotate(B, [BF|BF0]).

board_match({_A,S}, NS) ->
	lists:any(fun(SS) -> sets:is_subset(SS, NS) end, S).

run_common(V) ->
	N = persistent_term:get({?MODULE,V,numbers}),
	B0 = persistent_term:get({?MODULE,V,boards}),
	B = lists:map(fun(BB) ->
		A = sets:from_list(lists:flatten(BB)),
		S = to_board_sets(BB),
		{A,S}
	end, B0),
	{B,N}.

run1(V) ->
	{B,N} = run_common(V),
	run1(B, sets:new(), N).
run1(B, NS0, [X|NR]) ->
	NS = sets:add_element(X, NS0),
	case lists:search(fun(BB) -> board_match(BB, NS) end, B) of
		{value,{A,_S}} ->
			{lists:sum(sets:to_list(sets:subtract(A, NS))),X};
		false ->
			run1(B, NS, NR)
	end.

run2(V) ->
	{B,N} = run_common(V),
	run2(B, sets:new(), N).
run2(B0 = [{A0,_S0}|_], NS0, [X|NR]) ->
	NS = sets:add_element(X, NS0),
	case lists:filter(fun(BB) -> not board_match(BB, NS) end, B0) of
		[] ->
			{lists:sum(sets:to_list(sets:subtract(A0, NS))),X};
		B ->
			run2(B, NS, NR)
	end.
