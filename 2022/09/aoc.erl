%%% https://adventofcode.com/2022/day/9

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
-export([part2/0, part2/1]).

%-compile(export_all).

-record(state, {
	position	= {0,0}	:: position(),
	history		= []	:: list(position())
}).
-type position()		:: {integer(),integer()}.

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = binary:split(B, <<"\n">>, [global,trim]),
		persistent_term:put({?MODULE,K}, parse(L))
	end, [{example,"input.example"},{example2,"input.example2"},{input,"input"}]).

parse(L) ->
	[ {dir(D),binary_to_integer(S)} || <<D, " ", S/binary>> <- L ].

dir($U) -> up;
dir($D) -> down;
dir($L) -> left;
dir($R) -> right.

p2h(S = #state{ position = P, history = H}) ->
	S#state{ history = [P|H] }.

move(S = #state{ position = {X,Y} }, up) ->
	S#state{ position = {X,Y+1} };
move(S = #state{ position = {X,Y} }, down) ->
	S#state{ position = {X,Y-1} };
move(S = #state{ position = {X,Y} }, left) ->
	S#state{ position = {X-1,Y} };
move(S = #state{ position = {X,Y} }, right) ->
	S#state{ position = {X+1,Y} }.

drag(S = #state{ position = {X,Y} }, #state{ position = {HX,HY} }) ->
	drag2(S, {HX - X,HY - Y}).
drag2(S = #state{ position = {_X,_Y} }, {XX,YY}) when XX == 0 orelse abs(XX) == 1, YY == 0 orelse abs(YY) == 1 ->
	S;
drag2(S = #state{ position = {X,Y} }, {XX0,YY0}) ->
	XX = if abs(XX0) == 2 -> XX0 div 2; true -> XX0 end,
	YY = if abs(YY0) == 2 -> YY0 div 2; true -> YY0 end,
	S#state{ position = {X + XX, Y + YY} }.

%%%

part1() ->
	part1(example).
part1(example) ->
	13 = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	{_Head,_Tail = #state{ position = P, history = H }} = ropesim(L, #state{}, #state{}),
	length(lists:uniq([P|H])).

ropesim([{D,S}|L], H0, T0) ->
	{H,T} = lists:foldl(fun(_, {HH0,TT0}) ->
		HH = move(p2h(HH0), D),
		TT = drag(p2h(TT0), HH),
		{HH,TT}
	end, {H0,T0}, lists:seq(1, S)),
	ropesim(L, H, T);
ropesim([], H, T) ->
	{H,T}.

%%%

part2() ->
	part2(example).
part2(example) ->
	1 = run2(example);
part2(example2) ->
	36 = run2(example2);
part2(input) ->
	run2(input).

run2(V) ->
	L = persistent_term:get({?MODULE,V}),
	Knots = ropesim2(L, 10),
	#state{ position = P, history = H } = lists:last(Knots),
	length(lists:uniq([P|H])).

ropesim2(L, C) ->
	ropesim2(L, lists:duplicate(C, #state{}), []).
ropesim2([], R, []) ->
	R;
ropesim2(_L = [{_D,S}|LR], [], RR) when S == 1 ->	% not zero as this is *after* we have made the move
	ropesim2(LR, lists:reverse(RR), []);
ropesim2(_L = [{D,S}|LR], [], RR) ->
	ropesim2([{D,S-1}|LR], lists:reverse(RR), []);
ropesim2(L = [{D,_S}|_LR], [A0|R0], []) ->
	A = move(p2h(A0), D),
	R = lists:map(fun p2h/1, R0),
	ropesim2(L, R, [A]);
ropesim2(L, [B0|R], RR = [A|_]) ->
	B = drag(B0, A),
	ropesim2(L, R, [B|RR]).
