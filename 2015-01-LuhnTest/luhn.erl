-module (luhn).
-export ([check/1, test/0]).

-spec check(bitstring()) -> true | false.
check(PAN) ->
  check_luhn(reverse(PAN), {0,0}).

check_luhn(<<>>, {S1, S2}) -> (S1+S2) rem 10 =:= 0;
check_luhn(<<Odd:1/binary>>, {S1, S2}) ->
  check_luhn(<<>>, {S1 + binary_to_integer(Odd), S2});
check_luhn(<<Odd:1/binary, Even:1/binary, Rest/binary>>, {S1, S2}) ->
  check_luhn(
  	Rest,
  	{S1 + binary_to_integer(Odd), S2 + sum_digits(2*binary_to_integer(Even))}
  ).

reverse(List) -> binary:list_to_bin(lists:reverse(binary:bin_to_list(List))).

sum_digits(Number) when Number > 9 ->
  (Number rem 10) + 1;
sum_digits(Number) ->
  Number.

test() ->
  io:fwrite("49927398716 is ~p~n", [check(<<"49927398716">>)]),
  io:fwrite("49927398717 is ~p~n", [check(<<"49927398717">>)]),
  io:fwrite("1234567812345678 is ~p~n", [check(<<"1234567812345678">>)]),
  io:fwrite("1234567812345670 is ~p~n", [check(<<"1234567812345670">>)]).
