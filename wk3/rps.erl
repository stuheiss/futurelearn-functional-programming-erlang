-module(rps).
-export([play/1,echo/1,play_two/3,rock/1,no_repeat/1,const/1,enum/1,cycle/1,rand/1,val/1,tournament/2]).
-compile(export_all).


%
% play one strategy against another, for N moves.
%

play_two(StrategyL,StrategyR,N) ->
    play_two(StrategyL,StrategyR,[],[],N).

% tail recursive loop for play_two/3
% 0 case computes the result of the tournament

% FOR YOU TO DEFINE
% REPLACE THE dummy DEFINITIONS

play_two(_,_,PlaysL,PlaysR,0) ->
  T=tournament(PlaysL,PlaysR),
  io:format("Game: ~p~n",[T]),
  ok;

play_two(StrategyL,StrategyR,PlaysL,PlaysR,N) ->
  PlayL=StrategyL(PlaysR),
  PlayR=StrategyR(PlaysL),
  Result=result(PlayL,PlayR),
  io:format("Result: ~p~n", [Result]),
  play_two(StrategyL,StrategyR,[PlayL|PlaysL],[PlayR|PlaysR],N-1).

%
% interactively play against a strategy, provided as argument.
%

play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy,[]).

% tail recursive loop for play/1

play(Strategy,Moves) ->
    {ok,P} = io:read("Play: "),
    Play = expand(P),
    case Play of
      stop ->
	      io:format("Stopped~n");
      _ ->
        Result = result(Play,P2=Strategy(Moves)),
        %io:format("Result: ~p~n",[Result]),
        io:format("Result: ~p ~p ~p ~p~n",[Play,P2,Result,[Play|Moves]]),
        play(Strategy,[Play|Moves])
    end.

%
% auxiliary functions
%

% transform shorthand atoms to expanded form
    
expand(r) -> rock;
expand(p) -> paper;		    
expand(s) -> scissors;
expand(X) -> X.

% result of one set of plays

result(rock,rock) -> draw;
result(rock,paper) -> lose;
result(rock,scissors) -> win;
result(paper,rock) -> win;
result(paper,paper) -> draw;
result(paper,scissors) -> lose;
result(scissors,rock) -> lose;
result(scissors,paper) -> win;
result(scissors,scissors) -> draw.

% result of a tournament

tournament(PlaysL,PlaysR) ->
    lists:sum(
      lists:map(fun outcome/1,
        lists:zipwith(fun result/2,PlaysL,PlaysR))).

outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

% transform 0, 1, 2 to rock, paper, scissors and vice versa.

enum(0) -> rock;
enum(1) -> paper;
enum(2) -> scissors.

val(rock) -> 0;
val(paper) -> 1;
val(scissors) -> 2.

% give the play which the argument beats.
beats([]) -> rock;
beats([Last|_]) ->
  case Last of
    rock -> paper;
    paper -> scissors;
    scissors -> rock
  end.

%
% strategies.
%
echo([]) ->
     paper;
echo([Last|_]) ->
    Last.

rock(_) ->
    rock.



% FOR YOU TO DEFINE
% REPLACE THE dummy DEFINITIONS

no_repeat([]) ->
    paper;
no_repeat([X|_]) ->
    beats(X).

const(Play) ->
    Play.

cycle(Play) ->
    enum(length(Play) rem 3).

rand(_) ->
    enum(rand:uniform(3) - 1).

least_frequent(Play) ->
  {_,Atom}=lists:min([
                      {length(lists:filter(fun(X)->X==rock end, Play)),rock},
                      {length(lists:filter(fun(X)->X==paper end, Play)),paper},
                      {length(lists:filter(fun(X)->X==scissors end, Play)),scissors}
                      ]),
  Atom.

most_frequent(Play) ->
  {_,Atom}=lists:max([
                      {length(lists:filter(fun(X)->X==rock end, Play)),rock},
                      {length(lists:filter(fun(X)->X==paper end, Play)),paper},
                      {length(lists:filter(fun(X)->X==scissors end, Play)),scissors}
                      ]),
  Atom.

best(_Play) ->
    dummy.
