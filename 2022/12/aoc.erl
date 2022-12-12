%%% https://adventofcode.com/2022/day/12

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
%-export([part2/0, part2/1]).

-compile(export_all).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = binary:split(B, <<"\n">>, [global,trim]),
		persistent_term:put({?MODULE,K}, parse(L))
	end, [{example,"input.example"},{input,"input"}]).

% {Y,X} coordinates from top-left (like TV scanning!)
parse(L) ->
	M0 = array:from_list([ array:from_list(binary_to_list(LL)) || LL <- L], array:new()),
	{S,E} = array:foldl(fun(YY, AX, A) ->
		array:foldl(fun
			(XX, V, AA) when V == $S ->
				setelement(1, AA, {YY,XX});
			(XX, V, AA) when V == $E ->
				setelement(2, AA, {YY,XX});
			(_XX, _V, AA) ->
				AA
		end, A, AX)
	end, {undefined,undefined}, M0),
	M1 = coord_set(S, $a, M0),
	M = coord_set(E, $z, M1),
	{S,E,M}.

coord_get({Y,X}, A) -> array:get(X, array:get(Y, A)).
coord_set({Y,X}, V, A) -> array:set(Y, array:set(X, V, array:get(Y, A)), A).

%%%

part1() ->
	part1(example).
part1(example) ->
	31 = length(run1(example)) - 1;	% counting steps not positions so we want -1
part1(input) ->
	length(run1(input)) - 1.

run1(V) ->
	{S,E,M} = persistent_term:get({?MODULE,V}),
	astar(S, E, M).

% https://en.wikipedia.org/wiki/A*_search_algorithm
astar(S, E, M) ->
	FScoreS = astar_h(S, E, M),
	OpenSet = ordsets:from_list([{FScoreS,S}]),	% use ordsets so we can use a guard for the empty set
	CameFrom = array:new({default,array:new()}),
	GScore = coord_set(S, 0, array:new({default,array:new()})),
	FScore = coord_set(S, FScoreS, array:new({default,array:new()})),
	astar(M, E, OpenSet, CameFrom, GScore, FScore).
astar(Map, Goal, OpenSet, CameFrom, GScore, FScore) ->
	case OpenSet of
		[{_GoalFScore,Goal}|_] ->
			astar_path([Goal], CameFrom);
		[{_CurrentFScore,Current = {Y,X}}|_] ->
			Height = coord_get({Y,X}, Map),
			Neighbours = [ {N,coord_get(N, Map)} || N = {YY,XX} <- [{Y-1,X},{Y+1,X},{Y,X-1},{Y,X+1}], YY >= 0, YY < array:size(Map), XX >= 0, XX < array:size(array:get(0, Map)) ],
			astar(Map, Goal, tl(OpenSet), CameFrom, GScore, FScore, {Current,Height}, Neighbours);
		% goal never reached
		[] ->
			false
	end.
astar(Map, Goal, OpenSet, CameFrom, GScore, FScore, _Current, []) ->
	astar(Map, Goal, OpenSet, CameFrom, GScore, FScore);

astar(Map, Goal, OpenSet0, CameFrom0, GScore0, FScore0, Current = {{Y,X},H}, [_N = {{NY,NX},NH}|NR]) when NH - H < 2 ->
	TGScore = coord_get({Y,X}, GScore0) + 1,	% steps cost one always
	NGScore = coord_get({NY,NX}, GScore0),
	{OpenSet,CameFrom,GScore,FScore} = if
		TGScore < NGScore ->
			CameFrom1 = coord_set({NY,NX}, {Y,X}, CameFrom0),
			GScore1 = coord_set({NY,NX}, TGScore, GScore0),
			NFScore = TGScore + astar_h({NY,NX}, Goal, Map),
			FScore1 = coord_set({NY,NX}, NFScore, FScore0),
			OpenSet1 = case lists:keyfind({NY,NX}, 2, OpenSet0) of
				false ->
					ordsets:add_element({NFScore,{NY,NX}}, OpenSet0);
				_Else ->
					OpenSet0
			end,
			{OpenSet1,CameFrom1,GScore1,FScore1};
		true ->
			{OpenSet0,CameFrom0,GScore0,FScore0}
	end,
	astar(Map, Goal, OpenSet, CameFrom, GScore, FScore, Current, NR);
astar(Map, Goal, OpenSet, CameFrom, GScore, FScore, Current, [_N|NR]) ->
	astar(Map, Goal, OpenSet, CameFrom, GScore, FScore, Current, NR).
	
astar_h(_N = {NY,NX}, _E = {EY,EX}, _M) ->
	abs(EY - NY) + abs(EX - NX).

astar_path(Path = [C|_], CameFrom) ->
	case coord_get(C, CameFrom) of
		undefined ->
			Path;
		N ->
			astar_path([N|Path], CameFrom)
	end.

peek({Y,X}, M) when is_atom(M) ->
	peek({Y,X}, element(3, persistent_term:get({aoc,M})));
peek({Y,X}, M) ->
	lists:foreach(fun(YY) ->
		lists:foreach(fun(XX) ->
			io:format("~s",[[coord_get({YY,XX},M)]])
		end, lists:seq(max(0, X - 5), X + 5)),
		io:format("~n",[])
	end, lists:seq(max(0, Y - 5), Y + 5)).
