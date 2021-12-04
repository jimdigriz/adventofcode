%%% https://adventofcode.com/2021/day/2

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

part1() ->
	part1(example).
part1(example) ->
	PD = {P = 15,D = 10} = run1(example),
	{PD,P*D};
part1(input) ->
	PD = {P,D} = run1(input),
	{PD,P*D}.

part2() ->
	part2(example).
part2(example) ->
	PD = {P = 15,D = 60} = run2(example),
	{PD,P*D};
part2(input) ->
	PD = {P,D} = run2(input),
	{PD,P*D}.

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	run12(L, {0,0}).
run12([<<"forward ", X0/binary>>|R], {P,D}) ->
	X = binary_to_integer(X0),
	run12(R, {P+X,D});
run12([<<"up ", X0/binary>>|R], {P,D}) ->
	X = binary_to_integer(X0),
	run12(R, {P,D-X});
run12([<<"down ", X0/binary>>|R], {P,D}) ->
	X = binary_to_integer(X0),
	run12(R, {P,D+X});
run12([], PD = {_P,_D}) ->
	PD.

run2(V) ->
	L = persistent_term:get({?MODULE,V}),
	run22(L, {0,0,0}).
run22([<<"forward ", X0/binary>>|R], {P,D,A}) ->
	X = binary_to_integer(X0),
	run22(R, {P+X,D+(A*X),A});
run22([<<"up ", X0/binary>>|R], {P,D,A}) ->
	X = binary_to_integer(X0),
	run22(R, {P,D,A-X});
run22([<<"down ", X0/binary>>|R], {P,D,A}) ->
	X = binary_to_integer(X0),
	run22(R, {P,D,A+X});
run22([], {P,D,_A}) ->
	{P,D}.
