%%% https://adventofcode.com/2022/day/5

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
%-export([part2/0, part2/1]).

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
	[C0|L] = lists:reverse(binary:split(D0, <<"\n">>, [global,trim])),
	[CB|_] = lists:reverse(binary:split(C0, <<" ">>, [global,trim_all])),
	C = binary_to_integer(CB),
	A = array:new([{size,C},fixed,{default,[]}]),
	parse_drawing(L, 0, A).
parse_drawing([<<"[",N,"]", R0/binary>>|L], C, A0) ->
	BL = byte_size(R0),
	R = if BL > 0 -> binary:part(R0, 1, BL - 1); true -> R0 end,
	A = array:set(C, [N|array:get(C, A0)], A0),
	parse_drawing([R|L], C + 1, A);
parse_drawing([<<"   ", R0/binary>>|L], C, A) ->
	BL = byte_size(R0),
	R = if BL > 0 -> binary:part(R0, 1, BL - 1); true -> R0 end,
	parse_drawing([R|L], C + 1, A);
parse_drawing([<<>>|L], _C, A) ->
	parse_drawing(L, 0, A);
parse_drawing([], 0, A) ->
	A.

parse_instructions(I0) when is_binary(I0) ->
	I = [ binary:split(II, <<" ">>, [global,trim]) || II <- binary:split(I0, <<"\n">>, [global,trim]) ],
	[ {binary_to_integer(C),binary_to_integer(F) - 1,binary_to_integer(T) - 1} || [<<"move">>, C, <<"from">>, F, <<"to">>, T] <- I ].

execute([], A) ->
	A;
execute([{C,F,T}|I], A0) ->
	A = lists:foldl(fun(_CC, AA0) ->
		[V|VF] = array:get(F, AA0),
		AA1 = array:set(F, VF, AA0),
		VT = array:get(T, AA1),
		array:set(T, [V|VT], AA1)
	end, A0, lists:seq(1, C)),
	execute(I, A).

%%%

part1() ->
	part1(example).
part1(example) ->
	"CMZ" = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	{A0,I} = persistent_term:get({?MODULE,V}),
	A = execute(I, A0),
	array:foldr(fun(_II, [VV|_VR], RR) ->
		[VV|RR]
	end, [], A).
