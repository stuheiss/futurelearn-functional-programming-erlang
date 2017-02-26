-module('2.8').
-compile(export_all).

% pattern match in head
circles0([]) -> [];
circles0([{circle,_,_}=C|Xs]) -> [C|circles0(Xs)];
circles0([_|Xs]) -> circles0(Xs).

% pattern match in body on X
circles1([]) -> [];
circles1([X|Xs]) ->
  case X of
    % why does C bind since it's on the right side of =
    {circle,_,_}=C -> [C|circles1(Xs)];
    _ -> circles1(Xs)
  end.

% I prefer this
circles3([]) -> [];
circles3([X|Xs]) ->
  case X of
    {circle,_,_} -> [X|circles3(Xs)];
    _ -> circles3(Xs)
  end.

% pattern match in body on element
circles2([]) -> [];
circles2([X|Xs]) ->
  case erlang:element(1,X) of
    circle -> [X|circles2(Xs)];
    _ -> circles2(Xs)
  end.

% nub - remove duplicates
nub([]) -> [];
nub([X|Xs]) -> [X|nub(filter(fun(Y)->X=/=Y end,Xs))].

% median - middle element
median(Xs) ->
  L=length(Xs),
  I=L div 2,
  case L rem 2 of
    1 -> lists:nth(I + 1, Xs);
    0 -> (lists:nth(I, Xs) + lists:nth(I + 1, Xs)) / 2
  end.

% modes - return most common element(s)
modes([]) -> [];
modes(Xs) ->
  Frequencies=freq(Xs),
  {Counts,_}=lists:unzip(Frequencies),
  MaxCount=lists:max(Counts),
  ResultPairs=lists:filter(fun({N,_})->N==MaxCount end, Frequencies),
  {_,Result}=lists:unzip(ResultPairs),
  Result.


% generate list of {count, elem} for each elem in input list
freq([]) -> [];
freq([X|Xs]) ->
  N=1+length(filter(fun(Y)->X==Y end,Xs)),
  [{N,X} | freq(filter(fun(Y)->X=/=Y end, Xs))].

% map
map(_,[]) -> [];
map(F,[X|Xs]) ->
  [F(X)|map(F,Xs)].

% filter
filter(_,[]) -> [];
filter(F,[X|Xs]) ->
  case F(X) of
    true -> [X|filter(F,Xs)];
    _ -> filter(F,Xs)
  end.

% reduce

run_tests() ->
  [1,2]=filter(fun(X)->X<3 end,[1,2,3,4,5]),
  [2,4,6,8,10]=map(fun(X)->X*2 end,[1,2,3,4,5]),
  C1={circle,{1,2},2},
  C2={circle,{3,4},5},
  R={rectangle,{5,4},3,2},
  [C1,C2]=circles0([C1,R,C2]),
  [C1,C2]=circles1([C1,R,C2]),
  [C1,C2]=circles2([C1,R,C2]),
  [C1,C2]=circles3([C1,R,C2]),
  ok.
