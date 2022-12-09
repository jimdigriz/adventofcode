%%% https://adventofcode.com/2022/day/9

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
%-export([part2/0, part2/1]).

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
	end, [{example,"input.example"},{input,"input"}]).

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
drag2(S = #state{ position = {X,Y} }, {XX,YY}) when XX == 0 orelse abs(XX) == 2, YY == 0 orelse abs(YY) == 2 ->
	S#state{ position = {X + (XX div 2), Y + (YY div 2)} };
drag2(S = #state{ position = {X,Y} }, {XX0,YY0}) ->
	XX = if abs(XX0) == 1 -> XX0; true -> XX0 div 2 end,
	YY = if abs(YY0) == 1 -> YY0; true -> YY0 div 2 end,
	S#state{ position = {X + XX, Y + YY} }.

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

part1() ->
	part1(example).
part1(example) ->
	13 = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	L = persistent_term:get({?MODULE,V}),
	{_Head,_Tail = #state{ history = TH }} = ropesim(L, #state{}, #state{}),
	length(lists:uniq(TH)).
