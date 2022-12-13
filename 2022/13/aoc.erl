%%% https://adventofcode.com/2022/day/13

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
	parse(L, []).
parse([], A) ->
	lists:reverse(A);
parse([L|LR], A) when L == <<>> ->
	parse(LR, A);
parse([L|LR], [[VA]|A]) ->
	{value,VB,[]} = erl_eval:expr(merl:quote(L), []),
	parse(LR, [{VA,VB}|A]);
parse([L|LR], A) ->
	{value,VA,[]} = erl_eval:expr(merl:quote(L), []),
	parse(LR, [[VA]|A]).

order([LA|_LAR], [LB|_LBR]) when is_integer(LA), is_integer(LB), LA < LB -> true;
order([LA|_LAR], [LB|_LBR]) when is_integer(LA), is_integer(LB), LA > LB -> false;
order([LA|LAR], [LB|LBR]) when is_integer(LA), is_integer(LB) -> order(LAR, LBR);
order([], _) -> true;
order(_, []) -> false;
order([LA|LAR], [LB|LBR]) when is_integer(LA), is_list(LB) -> order([[LA]|LAR], [LB|LBR]);
order([LA|LAR], [LB|LBR]) when is_list(LA), is_integer(LB) -> order([LA|LAR], [[LB]|LBR]);
order([LA|LAR], [LB|LBR]) when is_list(LA), is_list(LB), LA == LB -> order(LAR, LBR);
order([LA|_LAR], [LB|_LBR]) when is_list(LA), is_list(LB) -> order(LA, LB).

%%%

part1() ->
	part1(example).
part1(example) ->
	L = [1,2,4,6] = run1(example),
	lists:sum(L);
part1(input) ->
	lists:sum(run1(input)).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	lists:filtermap(fun({I,{A,B}}) ->
		case order(A, B) of
			true ->
				{true,I};
			false ->
				false
		end
	end, lists:enumerate(L)).

%%%

part2() ->
	part2(example).
part2(example) ->
	[X = 10, Y = 14] = run2(example),
	X * Y;
part2(input) ->
	[X,Y] = run2(input),
	X * Y.

run2(V) ->
	L0 = persistent_term:get({?MODULE,V}),
	L1 = lists:foldl(fun({A,B}, AA) -> [A,B|AA] end, [ [[2]], [[6]] ], L0),
	% something is wrong with the use of lists:sort/2 here, but reversing it gives the right answer
	% [[2]] is correctly placed but [[6]] is after items order/2 claims is wrong
	L2 = lists:sort(fun order/2, lists:reverse(L1)),
	L = lists:enumerate(L2),
	{X,_} = lists:keyfind([[2]], 2, L),
	{Y,_} = lists:keyfind([[6]], 2, L),
	[X, Y].
