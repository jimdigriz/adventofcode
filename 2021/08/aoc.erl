%%% https://adventofcode.com/2021/day/8

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
-export([part2/0, part2/1]).

%-compile(export_all).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = parse(binary:split(B, <<"\n">>, [global,trim])),
		persistent_term:put({?MODULE,K}, L)
	end, [{example,"input.example"},{input,"input"}]).

parse(R) ->
	parse(R, []).
parse([], A) ->
	lists:reverse(A);
parse([L0|R], A) ->
	{U0,[<<"|">>|N0]} = lists:split(10, binary:split(L0, <<" ">>, [global,trim])),
	U = lists:map(fun(X) -> ordsets:from_list(binary_to_list(X)) end, U0),
	N = lists:map(fun(X) -> ordsets:from_list(binary_to_list(X)) end, N0),
	parse(R, [{U,N}|A]).

part1() ->
	part1(example).
part1(example) ->
	26 = run1(example);
part1(input) ->
	run1(input).

part2() ->
	part2(example).
part2(example) ->
	{61229,_A} = run2(example);
part2(input) ->
	run2(input).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	lists:foldl(fun({_U,N}, A) ->
		A + length(lists:filter(fun
			(X) when length(X) == 2; length(X) == 4; length(X) == 3; length(X) == 7 ->
				true;
			(_X) ->
				false
		end, N))
	end, 0, L).

run2(V) ->
	L = persistent_term:get({?MODULE,V}),
	run2(L, []).
run2([], A0) ->
	A = lists:reverse(A0),
	{lists:sum(A),A};
run2([{U,N}|L], A0) ->
	S0 = lists:foldl(fun(X, D) ->
		seg(X, U, D)
	end, #{}, [1,4,7,8,6,0,9,2,3,5]),
	S = maps:from_list(lists:zip(maps:values(S0),maps:keys(S0))),
	A = [list_to_integer(lists:foldr(fun(X, D) -> [$0 + maps:get(X, S)|D] end, [], N))|A0],
	run2(L, A).

seg(D, U, S) when D == 1 ->
	{value,Value} = lists:search(fun(X) -> length(X) == 2 end, U),
	S#{ D => Value };
seg(D, U, S) when D == 4 ->
	{value,Value} = lists:search(fun(X) -> length(X) == 4 end, U),
	S#{ D => Value };
seg(D, U, S) when D == 7 ->
	{value,Value} = lists:search(fun(X) -> length(X) == 3 end, U),
	S#{ D => Value };
seg(D, U, S) when D == 8 ->
	{value,Value} = lists:search(fun(X) -> length(X) == 7 end, U),
	S#{ D => Value };
seg(D, U, S = #{ 1 := S1 }) when D == 6 ->
	{value,Value} = lists:search(fun(X) -> length(X) == 6 andalso not ordsets:is_subset(S1, X) end, U),
	S#{ D => Value };
seg(D, U, S = #{ 4 := S4, 6 := S6 }) when D == 0 ->
	{value,Value} = lists:search(fun(X) -> length(X) == 6 andalso X =/= S6 andalso not ordsets:is_subset(S4, X) end, U),
	S#{ D => Value };
seg(D, U, S = #{ 4 := S4 }) when D == 9 ->
	{value,Value} = lists:search(fun(X) -> length(X) == 6 andalso ordsets:is_subset(S4, X) end, U),
	S#{ D => Value };
seg(D, U, S = #{ 9 := S9 }) when D == 2 ->
	{value,Value} = lists:search(fun(X) -> length(X) == 5 andalso not ordsets:is_subset(X, S9) end, U),
	S#{ D => Value };
seg(D, U, S = #{ 1 := S1 }) when D == 3 ->
	{value,Value} = lists:search(fun(X) -> length(X) == 5 andalso ordsets:is_subset(S1, X) end, U),
	S#{ D => Value };
seg(D, U, S = #{ 2 := S2, 3 := S3 }) when D == 5 ->
	{value,Value} = lists:search(fun(X) -> length(X) == 5 andalso X =/= S2 andalso X =/= S3 end, U),
	S#{ D => Value }.
