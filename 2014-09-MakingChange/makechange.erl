-module(makechange).
-export([count_change/2]).

count_change(0, _) -> 1;
count_change(_, []) -> 0; 
count_change(Amount, _) when Amount < 0 -> 0; 
count_change(Amount, [Coin|Rest]=Coins) ->
    count_change(Amount, Rest) + count_change(Amount - Coin, Coins).
