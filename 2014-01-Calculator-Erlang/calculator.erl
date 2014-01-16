-module(calculator).
%-export([add/0]).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_DELIMITERS, [",", "\n"]).

%% Author: Philip Clarke <send2philip@gmail.com>
%%
%% Solution to https://github.com/FunctionalKatas/Katas/blob/master/2014-January-Calculator.markdown
%%
%% To compile, install erlang (any version will do)
%% Start up the erlang shell
%%   prompt> erl
%% compile the module
%%   1> c(calculator).
%%   {ok, calculator}
%% Run the unit tests
%%   2> eunit:test(calculator).
%%   All 12 tests passed
%% Run the program
%%   3> calculator:add().
%%   Give me numbers: 1,2,3,4,
%%   10
%%
%% You can't enter new lines from the prompt (and hence you can't define a custom delimiter), 
%% however the unit tests show how these would be handled if read in e.g. from a file


add() ->
    Input = io:get_line("Give me numbers: "),
    convert_and_add(Input).

add(Integers) ->
    add(Integers, []).

add([], []) ->
    0;
add([], Negatives) ->
    %erlang:error({negative_numbers, []});
    erlang:error({negative_numbers, lists:reverse(Negatives)});
add([Num|Rest], Negatives) when Num < 0 ->
    add(Rest, [Num|Negatives]);
add([Num|Rest], Negatives) when Num > 1000 ->
    add(Rest, Negatives);
add([Num|Rest], Negatives) ->
    Num + add(Rest, Negatives).


convert("//" ++ Input) ->
    Pos = string:chr(Input, $\n),
    Delimiter = [string:substr(Input, 1, Pos - 1)],
    Remainder = string:substr(Input, Pos + 1),
    convert(Remainder, Delimiter ++ ?DEFAULT_DELIMITERS);
convert(Input) ->
    convert(Input, ?DEFAULT_DELIMITERS).

convert(Input, Delimiters) ->
    lists:foldl(fun(Delimiter, Acc) ->
                    string:join(string:tokens(Acc, Delimiter), ",")
                end,
                Input,
                Delimiters).

convert_and_add(Input) ->
    Numbers = convert(Input),
    Integers = [list_to_integer(Num) || Num <- string:tokens(Numbers, ",")],
    add(Integers).


%% Unit tests %%
convert_and_add_test_() ->
    [?_assertEqual(0, convert_and_add([])),
     ?_assertEqual(0, convert_and_add([$\n])),
     ?_assertEqual(0, convert_and_add([$,])),
     ?_assertEqual(1, convert_and_add("1")), %% same as convert_and_add([49])
     ?_assertEqual(3, convert_and_add("1,2")),
     ?_assertEqual(13, convert_and_add("1,2,10")),
     ?_assertEqual(13, convert_and_add("1,2\n10")),
     ?_assertEqual(1002, convert_and_add("2,1000")),
     ?_assertEqual(2, convert_and_add("2,1001")),
     ?_assertError({negative_numbers, [-1, -2]}, convert_and_add("1,-1,10,-2")),
     ?_assertEqual(3, convert_and_add("//;\n1;2")),
     ?_assertEqual(3, convert_and_add("//;;\n1;;2"))
     ].
