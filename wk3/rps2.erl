-module(rps2).
-compile(export_all).

-type rps_type():: rock | paper | scissors.
-type result_type():: draw | win | lose.

% Rock defeats scissors, because a rock will blunt a pair of scissors
% Paper defeats rock, because a paper can wrap up a rock
% Scissors defeat paper, because scissors cut paper
-spec beats(_) -> rps_type().
beats(rock) -> paper;
beats(paper) -> scissors;
beats(scissors) -> rock.

% lose(winner) -> loser.
-spec loses(_) -> rps_type().
loses(rock) -> scissors;
loses(paper) -> rock;
loses(scissors) -> paper.

% Define a function result which when applied to two plays gives the result,
% from the point of view of the first. For example:
% result(rock,paper) = lose.
% result can return win/lose/draw
-spec result(_,_) -> result_type().
result(rock,scissors) -> win;
result(rock,paper) -> lose;
result(rock,rock) -> draw;
result(paper,scissors) -> lose;
result(paper,paper) -> draw;
result(paper,rock) -> win;
result(scissors,scissors) -> draw;
result(scissors,paper) -> win;
result(scissors,rock) -> lose.

-spec outcome(result_type()) -> -1 | 0 | 1.
outcome(win) -> 1;
outcome(lose) -> -1;
outcome(draw) -> 0.

% A tournament is a series of rounds – each round is a single choice from the
% two players, which we’ll call left and right. Suppose that the choices are
% given as two lists; give the tournament result as an integer, so that the
% number counts the difference between the number of wins for left and right.
% A positive value is an overall win for left, a negative for right, and zero
% represents an overall draw. For instance:
% tournament([rock,rock,paper,paper],[rock,paper,scissors,rock] = -1
-spec tournament([rps_type()],[rps_type()]) -> number().
tournament(Left,Right) ->
  lists:sum(
    lists:map(fun outcome/1,
      lists:zipwith(fun result/2,Left,Right))).

enum(0) -> rock;
enum(1) -> paper;
enum(2) -> scissors.

val(rock) -> 0;
val(paper) -> 1;
val(scissors) -> 2.

% Strategies
echo([]) -> paper;
echo([Last|_]) -> Last.

rock(_) -> rock.

no_repeat([]) -> scissors;
no_repeat([X|_]) -> beats(X).

const(Play) -> fun(_) -> Play end.

cycle(Xs) -> enum(length(Xs) rem 3).

rand(_) -> enum(random:uniform(3) - 1).

play(Strategey) ->
  io:format("Rock - paper - scissors~n"),
  io:format("Play one of rock, paper, scissors, ...~n"),
  io:format("... r, p, s, stop, followed by '.'~n"),
  play(Strategey,[]).

play(Strategey,Moves) ->
  {ok,P} = io:read("Play: "),
  Play = expand(P),
  case Play of
    stop ->
      io:format("Stopped~n");
    _ ->
      Result=result(Play,Strategey(Moves)),
      io:format("Result: ~p~n",[Result]),
      play(Strategey,[Play|Moves])
    end.

expand(P) ->
  case P of
    r -> rock;
    p -> paper;
    s -> scissors
  end.

-spec run_tests() -> 'ok'.
run_tests() ->
  lose=result(rock,paper),
  win=result(paper,rock),
  lose=result(paper,scissors),
  win=result(scissors,paper),
  lose=result(scissors,rock),
  win=result(rock,scissors),
  draw=result(rock,rock),
  draw=result(paper,paper),
  draw=result(scissors,scissors),
  -1=tournament([rock,rock,paper,paper],[rock,paper,scissors,rock]),
  1=tournament([rock,scissors,paper,paper],[rock,paper,scissors,rock]),
  ok.
