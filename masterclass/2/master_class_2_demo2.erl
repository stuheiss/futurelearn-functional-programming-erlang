-module(master_class_2_demo2).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

area({square,X}) -> X*X;
area({rectangle,X,Y}) -> X*Y.
area() ->
  receive
    {From, {square, X}} ->
      From ! {self(), X*X};
    {From, {rectangle, X, Y}} ->
      From ! {self(), X*Y}
  end,
  area().

% Pid = spawn(master_class_2_demo2, area, []),
% Pid ! {self(), {square, 10}},
% receive
%   {Pid, Reply} ->
%     Reply
% end.



a1_test() ->
  ?assertEqual(9, area({square, 3})).
a2_test() ->
  ?assertEqual(16, area({rectangle, 2, 8})).
