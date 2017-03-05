-module(myrps).
-compile(export_all).

-type rps_type():: error | rock | paper | scissors.
-type result_type():: error | draw | win | lose.

% Rock defeats scissors, because a rock will blunt a pair of scissors
% Paper defeats rock, because a paper can wrap up a rock
% Scissors defeat paper, because scissors cut paper
% beat(loser) -> winner.
-spec beat(_) -> rps_type().
beat(rock) -> paper;
beat(paper) -> scissors;
beat(scissors) -> rock;
beat(_) -> error.

% lose(winner) -> loser.
-spec lose(_) -> rps_type().
lose(rock) -> scissors;
lose(paper) -> rock;
lose(scissors) -> paper;
lose(_) -> error.

% Define a function result which when applied to two plays gives the result,
% from the point of view of the first. For example:
% result(rock,paper) = lose.
% result can return win/lose/draw
-spec result(_,_) -> result_type().
result(X,Y) ->
  case {beat(X), beat(Y)} of
    {error,_} -> error;
    {_,error} -> error;
    {Y,_} -> lose;
    {_,X} -> win;
    {_,_} -> draw
  end.

-spec result_to_int(_,_) -> -1 | 0 | 1.
result_to_int(X,Y) ->
  case result(X,Y) of
    win -> 1;
    lose -> -1;
    _ -> 0
  end.

% A tournament is a series of rounds – each round is a single choice from the
% two players, which we’ll call left and right. Suppose that the choices are
% given as two lists; give the tournament result as an integer, so that the
% number counts the difference between the number of wins for left and right.
% A positive value is an overall win for left, a negative for right, and zero
% represents an overall draw. For instance:
% tournament([rock,rock,paper,paper],[rock,paper,scissors,rock] = -1
-spec tournament([rps_type()],[rps_type()]) -> number().
tournament(Left,Right) ->
  lists:sum(lists:zipwith(fun result_to_int/2,Left,Right)).

-spec run_tests() -> 'ok'.
run_tests() ->
  paper=beat(rock),
  scissors=beat(paper),
  rock=beat(scissors),
  lose=result(rock,paper),
  win=result(paper,rock),
  lose=result(paper,scissors),
  win=result(scissors,paper),
  lose=result(scissors,rock),
  win=result(rock,scissors),
  draw=result(rock,rock),
  draw=result(paper,paper),
  draw=result(scissors,scissors),
  error=result(rock,water),
  error=result(water,rock),
  -1=tournament([rock,rock,paper,paper],[rock,paper,scissors,rock]),
  1=tournament([rock,scissors,paper,paper],[rock,paper,scissors,rock]),
  ok.
