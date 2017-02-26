-module('1.14').
-export([is_zero/1,xOr/2]).

is_zero(0) ->
    true;
is_zero(_) ->
    false.

%% xOr(true,false) ->
%%     true;
%% xOr(false,true) ->
%%     true;
%% xOr(_,_) ->
%%     false.

xOr(X,X) ->
    false;
xOr(_,_) ->
    true.
