%%% https://adventofcode.com/2022/day/7

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
-export([part2/0, part2/1]).

%-compile(export_all).

-define(TOTAL, 70000000).
-define(NEED, 30000000).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = binary:split(B, <<"\n">>, [global,trim]),
		persistent_term:put({?MODULE,K}, parse(L))
	end, [{example,"input.example"},{input,"input"}]).

parse(L) ->
	parse(L, [<<"/">>], gb_trees:empty()).
parse([<<"$ cd /">>|R], _CWD, FS) ->
	parse(R, [<<"/">>], FS);
parse([<<"$ cd ..">>|R], CWD, FS) ->
	parse(R, tl(CWD), FS);
parse([<<"$ cd ", WD/binary>>|R], CWD, FS) ->
	parse(R, [WD|CWD], FS);
parse([<<"$ ls">>|R], CWD, FS) ->
	parse(R, CWD, FS);
parse([<<"dir ", _N/binary>>|R], CWD, FS) ->
	parse(R, CWD, FS);
parse([C = <<X, _/binary>>|R], CWD, FS0) when X >= $0, X =< $9 ->
	[S0,N] = binary:split(C, <<" ">>),
	S = binary_to_integer(S0),
	FS = gb_trees:insert([{file,N}|CWD], S, FS0),
	parse(R, CWD, FS);
parse([], _CWD, FS) ->
	FS.

get_dirs({_K = [{file,_FF}|DD],SS,I}, FS0) ->
	FS = get_dirs2(DD, SS, FS0),
	get_dirs(gb_trees:next(I), FS);
get_dirs({_K,_V,I}, D) ->
	get_dirs(gb_trees:next(I), D);
get_dirs(none, D) ->
	D.

get_dirs2([], _S, FS) ->
	FS;
get_dirs2(D, S, FS0) ->
	{SS,FS} = case lists:keytake(D, 1, FS0) of
		{value,{D,SSS},FS1} ->
			{S + SSS,FS1};
		false ->
			{S,FS0}
	end,
	get_dirs2(tl(D), S, [{D,SS}|FS]).

%%%

part1() ->
	part1(example).
part1(example) ->
	95437 = run1(example);
part1(input) ->
	run1(input).

run1(V) ->
	T = persistent_term:get({?MODULE,V}),
	D0 = get_dirs(gb_trees:next(gb_trees:iterator(T)), []),
	D = lists:filtermap(fun
		({_DD,SS}) when SS =< 100000 ->
			{true,SS};
		({_DD,_SS}) ->
			false
	end, D0),
	lists:sum(D).

%%%

part2() ->
	part2(example).
part2(example) ->
	24933642 = run2(example);
part2(input) ->
	run2(input).

run2(V) ->
	T = persistent_term:get({?MODULE,V}),
	D = get_dirs(gb_trees:next(gb_trees:iterator(T)), []),
	Free = ?TOTAL - element(2, lists:keyfind([<<"/">>], 1, D)),
	[{_DD,SS}|_] = lists:dropwhile(fun({_DDD,SSS}) ->
		(Free + SSS) < ?NEED
	end, lists:keysort(2, D)),
	SS.
