-module('2.9').
-compile(export_all).

% Transforming list elements
% Define an Erlang function double/1 to double the elements of a list of numbers.
double(Xs) -> lists:map(fun(X)->X*2 end, Xs).

% Filtering lists
% Define a function evens/1 that extracts the even numbers from a list of integers.
evens(Xs) -> lists:filter(fun(X)->X rem 2 == 0 end, Xs).

% Going further
% If you want to try some other recursions on lists try to define functions to give:

% the median of a list of numbers: this is the middle element when the list is ordered (if the list is of even length you should average the middle two)
median(Xs) when Xs=/=[] ->
  L=length(Xs),
  I=L div 2,
  case L rem 2 of
    1 -> lists:nth(I + 1, Xs);
    0 -> (lists:nth(I, Xs) + lists:nth(I + 1, Xs)) / 2
  end.

% the modes of a list of numbers: this is a list consisting of the numbers that occur most frequently in the list; if there is is just one, this will be a list with one element only
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
  N=1+length(lists:filter(fun(Y)->X==Y end,Xs)),
  [{N,X} | freq(lists:filter(fun(Y)->X=/=Y end, Xs))].


% In doing this you might find it useful to think of other functions that you could define to help you solve these problems, such as a function to sort a list, or to work out how many times a value occurs in a particular list.
%
% Once you have written your solutions, particularly for the latter questions, you might like to discuss your approach to solving the problems with other participants using the comments on this step.

run_tests() ->
  [2,4,6,8,10]=double([1,2,3,4,5]),
  [2,4,6,8,10]=evens([1,2,3,4,5,6,7,8,9,10]),
  3=median([1,2,3,4,5]),
  3.5=median([1,2,3,4,5,6]),
  [3]=modes([1,2,2,3,3,3]),
  [2,3]=modes([1,2,2,2,3,3,3]),
  ok.
