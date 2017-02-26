-module('2.18').
-compile(export_all).

% Consolidation: functions over lists
% The aim of these optional exercises is to help you to consolidate your work on defining functions over lists, if you would like to do some more practice.
%
% In the next activity, we’ll go on to look at programming something larger-scale.
%
% Joining lists together
% Here we consider two of the principal functions over lists. The ++ operator joins two lists together, and lists:concat/1 joins a list of lists into a single list. For example:
%
% "hel"++"lo" = "hello"
% lists:concat(["goo","d","","by","e"]) = "goodbye"
% Write your own definitions of these functions. In the case of ++ you’ll need to define a function - say, join/2, as you can’t define your own operators in Erlang.
%
% Hint: Think about how you could use join (or ++) in the definition of concat.
join2([],Ys) -> Ys;
join2(Xs,[]) -> Xs;
join2(Xs,Ys) -> join2(Xs,Ys,[]).

join2([],[],Acc) -> lists:reverse(Acc);
join2(Xs,Ys,[]) -> join2([],Ys,lists:reverse(Xs));
join2([],[Y|Ys],Acc) -> join2([],Ys,[Y|Acc]).

concat([]) -> [];
concat([X|Xs]) -> join2(X,concat(Xs)).

% Testing membership
% Define a function member/2 that tests whether its first argument is a member of its second argument, which is a list. For example:
%
% member(2,[2,0,0,1]) = true
% member(20,[2,0,0,1]) = false
member(_,[]) -> false;
member(X,[Y|Ys]) ->
  case X==Y of
    true -> true;
    false -> member(X,Ys)
  end.


% Sorting lists
% A list can be sorted in a number of ways, including these algorithms described informally:
%
% Merge sort: divide the list into two halves of (approximately) equal length, sort them (recursively) and then merge the results.
%
% Quicksort: split the list into two according to whether the items are smaller than (or equal to) or larger than the pivot, often taken to be the head element of the list; sort the two halves and join the results together.
%
% Insertion sort: sort the tail of the list and then insert the head of the list in the correct place.
%
% Try to implement each of these sorting algorithms in Erlang.

% mergesort
mergesort([]) -> [];
mergesort([X]) -> [X];
mergesort(Xs) ->
  {L,R}=lists:split(length(Xs) div 2, Xs),
  mergesort(mergesort(L), mergesort(R)).
% merge sublists
mergesort([], []) -> [];
mergesort(Xs, []) -> Xs;
mergesort([], Ys) -> Ys;
mergesort([X|Xs], [Y|Ys]) ->
  case X<Y of
    true -> [X|mergesort(Xs,[Y|Ys])];
    false -> [Y|mergesort([X|Xs],Ys)]
  end.

% quicksort
quicksort([]) -> [];
quicksort([X|Xs]) ->
  {L,R}=lists:partition(fun(Y)->Y=<X end,Xs),
  quicksort(L) ++ [X] ++ quicksort(R).

% insertion sort
insertionsort([]) -> [];
insertionsort([X]) -> [X];
insertionsort([X|Xs]) ->
  insert(X,insertionsort(Xs)).

insert(X,[]) -> [X];
insert(X,[Y|Ys]) ->
  case X=<Y of
    true -> [X,Y|Ys];
    false -> [Y|insert(X,Ys)]
  end.



% Permutations
% A permutation of a list xs consists of the same elements in a (potentially) different order. Define a function that gives all the permutations of a list, in some order. For example:
%
% perms([]) = [[]]
% perms([1,2,3]) = [[1,2,3],[2,3,1],[3,1,2],[2,1,3],[1,3,2],[3,2,1]]
% Remember that you can use the comments on this step to ask questions about these exercises, to get some help if you would like some, or to discuss your different strategies for solving them.
perms([]) -> [[]];
perms(Xs) ->
  [[X|Y] || X <- Xs, Y <- perms(Xs -- [X])].

perms2([]) -> [[]];
perms2(Xs) -> perms2(Xs,Xs,[]).
perms2([],_,Acc) -> Acc;
% Xs is original list which is needed at every level of recursion
perms2([Y|Ys],Xs,Acc) ->
  % map over each sublist of Xs with Y removed consing Y onto the sublist
  MoreResults=lists:map(fun(PartialResult)->[Y|PartialResult] end, perms(Xs--[Y])),
  % add new results to running total and recurse on remainder of Ys
  perms2(Ys,Xs,MoreResults++Acc).

run_tests() ->
  []=join2([],[]),
  "abc"=join2("abc",[]),
  "def"=join2([],"def"),
  "abcdef"=join2("abc","def"),
  []=concat([]),
  "abcdefghi"=concat(["abc","def","ghi"]),
  "defghi"=concat(["","def","ghi"]),
  "abcghi"=concat(["abc","","ghi"]),
  "abcdef"=concat(["abc","def",""]),
  true=member(1,[1,2,3]),
  false=member(4,[1,2,3]),
  false=member(1,[]),
  []=quicksort([]),
  [3]=quicksort([3]),
  [3,4]=quicksort([3,4]),
  [3,4]=quicksort([4,3]),
  [1,2,3,4,5]=quicksort([1,3,4,2,5]),
  []=mergesort([]),
  [3]=mergesort([3]),
  [3,4]=mergesort([3,4]),
  [3,4]=mergesort([4,3]),
  [1,2,3,4,5]=mergesort([1,3,4,2,5]),
  []=insertionsort([]),
  [3]=insertionsort([3]),
  [3,4]=insertionsort([3,4]),
  [3,4]=insertionsort([4,3]),
  [1,2,3,4,5]=insertionsort([1,3,4,2,5]),
  [[]]=perms([]),
  [[]]=perms2([]),
  [[1]]=perms([1]),
  [[1]]=perms2([1]),
  [[1,2],[2,1]]=lists:sort(perms([1,2])),
  [[1,2],[2,1]]=lists:sort(perms2([1,2])),
  [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]=lists:sort(perms([1,2,3])),
  [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]=lists:sort(perms2([1,2,3])),
  ok.
