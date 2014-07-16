-module (tictactoe).
-export ([won/1]).

% Board = [empty, empty, empty, zero, x, empty, empty, empty, empty]

won(Board) ->
	% We document all the possible solutions
	PossibleSolutions = [[1,2,3], [4,5,6], [7,8,9], [1,4,7], [2,5,8], [3,6,9], [1,5,9], [3,5,7]],
	% check each solution
	FoundSolutions = lists:map(fun(X) ->
		check_solution(X, Board)
	end, PossibleSolutions),
	case lists:filter(fun(X) -> X =/= false end, FoundSolutions) of
		[]     -> false;
		[zero] -> zero;
		[x]    -> x
	end.

% Check if the possible solution is valid
check_solution(Keys, Board) ->
	is_solution(lists:map(fun(X) -> lists:nth(X, Board) end, Keys)).

is_solution([zero, zero, zero]) -> zero;
is_solution([x, x, x]) -> x;
is_solution(_) -> false.
