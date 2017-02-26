-module('2.3').
-export([expressions/0]).

expressions() ->
A = [1,2,4,1,3],
[2,3|A],
[97,104,97,33],
[2|[1,3|[4|[]]]],
[B|Bs] = [2,3|A],
A = tl(Bs),
[C,C|_] = [2,2,3,3,4],
C,
io:format("~p~n",[C]),

C1=case [2,3,4] of
 [X1,Y1|_] -> X1+Y1;
 [S1] -> S1;
 _ -> 0
end,
io:format("~p~n",[C1]),

C2=case [6] of
 [X2,Y2|_] -> X2+Y2;
 [S2] -> S2;
 _ -> 0
end,
io:format("~p~n",[C2]),

C3=case [] of
 [X3,Y3|_] -> X3+Y3;
 [S3] -> S3;
 _ -> 0
end,
io:format("~p~n",[C3]),
ok.

