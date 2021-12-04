%%% https://adventofcode.com/2021/day/3

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
-export([part2/0, part2/1]).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = binary:split(B, <<"\n">>, [global,trim_all]),
		persistent_term:put({?MODULE,K}, L)
	end, [{example,"input.example"},{input,"input"}]).

part1() ->
	part1(example).
part1(example) ->
	GE = {G = 22,E = 9} = run1(example),
	{GE,G*E};
part1(input) ->
	GE = {G,E} = run1(input),
	{GE,G*E}.

part2() ->
	part2(example).
part2(example) ->
	OC = {O = 23,C = 10} = run2(example),
	{OC,O*C};
part2(input) ->
	OC = {O,C} = run2(input),
	{OC,O*C}.

run1(V) ->
	L0 = persistent_term:get({?MODULE,V}),
	L = lists:map(fun binary_to_list/1, L0),
	A = [ 0 || _ <- lists:seq(1, length(hd(L))) ],
	run12(L, A, 0).
run12([H|R], A0, C) ->
	A = lists:map(fun({X,Y}) -> (X-$0) + Y end, lists:zip(H, A0)),
	run12(R, A, C + 1);
run12([], A, C) ->
	erlang:delete_element(3, lists:foldr(fun
		(X, {G,E,S}) when X > (C div 2) ->
			{G+S,E,2*S};
		(_X, {G,E,S}) ->
			{G,E+S,2*S}
	end, {0,0,1}, A)).

run2(V) ->
	L0 = persistent_term:get({?MODULE,V}),
	L = lists:map(fun binary_to_list/1, L0),
	{run22({L,L}, {$1,$0}),run22({L,L}, {$0,$1})}.
run22({L0 = [_,_|_],LL0}, M = {M1,M2}) ->
	{A,C} = lists:foldl(fun([X|_], {A,C}) ->
		{A+X-$0,C+1}
	end, {0,0}, LL0),
	V = if A > (C/2); A == (C/2) -> M1; true -> M2 end,
	run22(lists:unzip(lists:filtermap(fun
		({X,Y}) when hd(Y) == V ->
			{true,{X,tl(Y)}};
		({_X,_Y}) ->
			false
	end, lists:zip(L0,LL0))), M);
run22({_L0 = [V],_LL}, _M) ->
	element(1, lists:foldr(fun (X,{A,S}) -> {A+((X-$0)*S),2*S} end, {0,1}, V)).
