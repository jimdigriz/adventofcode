%%% https://adventofcode.com/2022/day/11

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
%-export([part2/0, part2/1]).

%-compile(export_all).

-include_lib("syntax_tools/include/merl.hrl").

-record(monkey, {
	id		:: non_neg_integer(),
	items		:: list(non_neg_integer()),
	oper		:: merl:tree(),
	test		:: merl:tree(),

	count	= 0	:: non_neg_integer()
}).

init() ->
	lists:foreach(fun({K,F}) ->
		{ok,B} = file:read_file(atom_to_list(?MODULE) ++ "." ++ F),
		L = binary:split(B, <<"\n">>, [global,trim]),
		persistent_term:put({?MODULE,K}, parse(L))
	end, [{example,"input.example"},{input,"input"}]).

parse(L) ->
	parse(L, []).
parse([], M) ->
	lists:reverse(M);
parse([<<>>|L], M) ->
	parse(L, M);
parse([<<"Monkey ", NB0/binary>>|L], M) ->
	<<NB:(byte_size(NB0) - 1)/binary, ":">> = NB0,
	N = binary_to_integer(NB),
	parse(L, [#monkey{ id = N }|M]);
parse([<<"  Starting items: ", IB/binary>>|L], [M|MR]) ->
	I = [ binary_to_integer(X) || X <- binary:split(IB, <<", ">>, [global]) ],
	parse(L, [M#monkey{ items = I }|MR]);
parse([<<"  Operation: new = ", O0/binary>>|L], [M|MR]) ->
	O = binary_to_list(binary:replace(O0, <<"old">>, <<"_@old">>, [global])),
%io:format("~p~n",[O]),
	parse(L, [M#monkey{ oper = O }|MR]);
parse([<<"  Test: divisible by ", TDB/binary>>,TT,TF|L], [M|MR]) ->
	TD = binary_to_list(TDB),
	<<"    If true: throw to monkey ", TMNB/binary>> = TT,
	<<"    If false: throw to monkey ", FMNB/binary>> = TF,
	TMN = binary_to_list(TMNB),
	FMN = binary_to_list(FMNB),
	T = "if _@worry rem " ++ TD ++ " == 0 -> " ++ TMN ++ "; true -> " ++ FMN ++ " end",
%io:format("~p~n",[T]),
	parse(L, [M#monkey{ test = T }|MR]).

%%%

part1() ->
	part1(example).
part1(example) ->
	[X = 105, Y = 101|_] = run1(example),
	X * Y;
part1(input) ->
	[X,Y|_] = run1(input),
	X * Y.

run1(V) ->
	ML = persistent_term:get({?MODULE,V}),
	run1(ML, 20).

run1(ML, 0) ->
	lists:reverse(lists:sort([ C || #monkey{ count = C } <- ML ]));
run1(ML0, C) ->
	ML = lists:foldl(fun(X, ML1) ->
		M = lists:keyfind(X, #monkey.id, ML1),
		ML3 = lists:foldl(fun(I, ML2) ->
			{value,W0,[]} = erl_eval:expr(erl_syntax:revert(?Q(M#monkey.oper, [{old,merl:term(I)}])), []),
			W = W0 div 3,
			{value,O,[]} = erl_eval:expr(erl_syntax:revert(?Q(M#monkey.test, [{worry,merl:term(W)}])), []),
			MO = #monkey{ items = MI } = lists:keyfind(O, #monkey.id, ML2),
			lists:keyreplace(O, #monkey.id, ML2, MO#monkey{ items = MI ++ [W] })
		end, ML1, M#monkey.items),
		lists:keyreplace(X, #monkey.id, ML3, M#monkey{ items = [], count = M#monkey.count + length(M#monkey.items) })
	end, ML0, lists:seq(0, length(ML0) - 1)),
	run1(ML, C - 1).
