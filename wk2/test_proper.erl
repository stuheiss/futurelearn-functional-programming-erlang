-module(test_proper).
-export([take/2]).
-include_lib("proper/include/proper.hrl").

take(_, []) -> [];
take(0, _) -> [];
take(N, [X|Xs]) when N>0 -> [X|take(N-1,Xs)].

%take_1_test() -> ?assertEqual(take(0,[]),[]).
%take_2_test() -> ?assertEqual(take(0,[3,4,5]),[]).
%take_3_test() -> ?assertEqual(take(1,[3,4,5]),[3]).
%take_4_test() -> ?assertEqual(take(2,[3,4,5]),[3,4]).
%take_5_test() -> ?assertEqual(take(3,[3,4,5]),[3,4,5]).
%take_6_test() -> ?assertEqual(take(4,[3,4,5]),[3,4,5]).
%take_7_test() -> ?assertEqual(take(5,[3,4,5]),[3,4,5]).
%take_8_test() -> ?assertEqual(take(1,[3,4,5]),[3,4,5]).

take_prop2() ->
  ?FORALL(N, nat(),
          ?FORALL(Xs, list(nat()), length(take(N, Xs)) == N)).


