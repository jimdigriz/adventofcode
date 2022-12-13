%%% https://adventofcode.com/2022/day/13

-module(aoc).

-on_load(init/0).

-export([part1/0, part1/1]).
%-export([part2/0, part2/1]).

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
	lists:filtermap(fun run12/1, lists:enumerate(L)).

run12({I,{[LA|_LAR],[LB|_LBR]}}) when is_integer(LA), is_integer(LB), LA < LB -> {true,I};
run12({_I,{[LA|_LAR],[LB|_LBR]}}) when is_integer(LA), is_integer(LB), LA > LB -> false;
run12({I,{[LA|LAR],[LB|LBR]}}) when is_integer(LA), is_integer(LB) -> run12({I,{LAR,LBR}});
run12({I,{[],_}}) -> {true,I};
run12({_I,{_,[]}}) -> false;
run12({I,{[LA|LAR],[LB|LBR]}}) when is_integer(LA), is_list(LB) -> run12({I,{[[LA]|LAR],[LB|LBR]}});
run12({I,{[LA|LAR],[LB|LBR]}}) when is_list(LA), is_integer(LB) -> run12({I,{[LA|LAR],[[LB]|LBR]}});
run12({I,{[LA|LAR],[LB|LBR]}}) when is_list(LA), is_list(LB), LA == LB -> run12({I,{LAR,LBR}});
run12({I,{[LA|_LAR],[LB|_LBR]}}) when is_list(LA), is_list(LB) -> run12({I,{LA,LB}}).
