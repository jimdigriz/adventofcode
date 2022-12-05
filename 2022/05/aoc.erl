%%% https://adventofcode.com/2022/day/5

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
-export([part2/0, part2/1]).

%-compile(export_all).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		[D,I0] = binary:split(B, <<"\n\n">>, [trim]),
		A = parse_drawing(D),
		I = parse_instructions(I0),
		persistent_term:put({?MODULE,K}, {A,I})
	end, [{example,"input.example"},{input,"input"}]).

parse_drawing(D0) ->
	[C0|L0] = lists:reverse(binary:split(D0, <<"\n">>, [global,trim])),
	[CB|_] = lists:reverse(binary:split(C0, <<" ">>, [global,trim_all])),
	C = binary_to_integer(CB),
	A = array:new([{size,C},fixed,{default,[]}]),
	L = [ <<" ", LL/binary>> || LL <- L0 ],
	parse_drawing(L, 0, A).
parse_drawing([<<" [",N,"]", R/binary>>|L], C, A0) ->
	A = array:set(C, [N|array:get(C, A0)], A0),
	parse_drawing([R|L], C + 1, A);
parse_drawing([<<"    ", R/binary>>|L], C, A) ->
	parse_drawing([R|L], C + 1, A);
parse_drawing([<<>>|L], _C, A) ->
	parse_drawing(L, 0, A);
parse_drawing([], 0, A) ->
	A.

parse_instructions(I0) when is_binary(I0) ->
	I = [ binary:split(II, <<" ">>, [global,trim]) || II <- binary:split(I0, <<"\n">>, [global,trim]) ],
	[ {binary_to_integer(C),binary_to_integer(F) - 1,binary_to_integer(T) - 1} || [<<"move">>, C, <<"from">>, F, <<"to">>, T] <- I ].

%%%

part1() ->
	part1(example).
part1(example) ->
	"CMZ" = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	{A0,I} = persistent_term:get({?MODULE,V}),
	A = execute1(I, A0),
	array:foldr(fun(_II, [VV|_VR], RR) ->
		[VV|RR]
	end, [], A).

execute1([], A) ->
	A;
execute1([{C,F,T}|I], A0) ->
	A = lists:foldl(fun(_CC, AA0) ->
		[V|VF] = array:get(F, AA0),
		AA1 = array:set(F, VF, AA0),
		VT = array:get(T, AA1),
		array:set(T, [V|VT], AA1)
	end, A0, lists:seq(1, C)),
	execute1(I, A).

%%%

part2() ->
	part2(example).
part2(example) ->
	"MCD" = run2(example);
part2(input) ->
	run2(input).

run2(V) ->
	{A0,I} = persistent_term:get({?MODULE,V}),
	A = execute2(I, A0),
	array:foldr(fun(_II, [VV|_VR], RR) ->
		[VV|RR]
	end, [], A).

execute2([], A) ->
	A;
execute2([{C,F,T}|I], A0) ->
	{V,VF} = lists:split(C, array:get(F, A0)),
	A1 = array:set(F, VF, A0),
	VT = array:get(T, A1),
	A = array:set(T, V ++ VT, A1),
	execute2(I, A).
