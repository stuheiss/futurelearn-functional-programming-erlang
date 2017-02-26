-module('2.15').
-compile(export_all).

% palindrome("Madam I\'m Adam") = true

palindrome(Xs) ->
  S=lists:filter(fun(X)->(X>=$A andalso X=<$Z) end, string:to_upper(Xs)),
  S==lists:reverse(S).

reverse(Xs) -> reverse(Xs,[]).
reverse([],Acc) -> Acc;
reverse([X|Xs],Acc) -> reverse(Xs,[X|Acc]).

run_tests() ->
  true=palindrome("Madam I\'m Adam"),
  false=palindrome("ABC"),
  ok.
