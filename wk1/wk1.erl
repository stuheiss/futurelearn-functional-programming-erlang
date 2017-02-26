-module('wk1').
-export([run_tests/0,bits/1,bits2/1,enclose/1,perimeter/1,area/1]).
% 1.24 Pulling it all together

% Shapes
% {circle, R}
% {rectangle, H, W}
% {triangle, A, B, C}
% Define a function perimeter/1 which takes a shape and returns the perimeter of the shape.
perimeter({circle, R}) ->
  math:pi() * 2 * R;
perimeter({rectangle, H, W}) ->
  2 * (H+W);
perimeter({triangle, A, B, C}) ->
  A+B+C.

% Choose a suitable representation of triangles, and augment area/1 and perimeter/1 to handle this case too.
area({circle, R}) ->
  math:pi() * R * R;
area({rectangle, H, W}) ->
  H * W;
% Heron's formula to calculate area of triangle
area({triangle, A, B, C}) ->
  S=(A+B+C) / 2,
  math:sqrt(S*(S-A)*(S-B)*(S-C)).

% Define a function enclose/1 that takes a shape an returns the smallest enclosing rectangle of the shape.
% For triangle shape, choose the longest side to be the Base
% Calculate the Height given Base and Area
%   Area = (Base.Height)/2
%   Height = (2.Area)/Base
% Enclosing rectangle will be size Base x Height

% case rectangle is itself
enclose(Rect = {rectangle, _H, _W}) ->
  Rect;
% case circle is rectangle size 2.R x 2.R
enclose({circle, R}) ->
  {rectangle, 2*R, 2*R};
% case triangle is rectangle size Base x Height
enclose(Tri = {triangle, A, B, C}) ->
  Base = max(max(A,B), max(B,C)),
  Height = (2 * area(Tri)) / Base,
  {rectangle, Base, Height}.

% Summing the bits
% Define a function bits/1 that takes a positive integer N and returns the sum of the bits in the binary representation. For example bits(7) is 3 and bits(8) is 1.
% See whether you can make both a direct recursive and a tail recursive definition.

% direct recursive
bits2(0) ->
  0;
bits2(N) ->
  bits2(N div 2) + (N rem 2).

% tail recursive
bits(N) when N>=0 ->
  bits(N,0).
bits(0,A) ->
  A;
bits(N,A) ->
  bits(N div 2, A+(N rem 2)).

% Which do you think is better? Why?
% I like both solutions. Both are easy to read.
% The tail recursive version has the advantange of constant stack space.

% tests
run_tests() ->
  ok = bits_test(),
  ok = bits2_test(),
  ok = perimeter_test(),
  ok = area_test(),
  ok = enclose_test(),
  ok.
perimeter_test() ->
  14 = perimeter({rectangle,3,4}),
  PC = math:pi() * 3 * 2, PC = perimeter({circle,3}),
  12 = perimeter({triangle,3,4,5}),
  ok.
area_test() ->
  12 = area({rectangle,3,4}),
  AC = math:pi() * 3 * 3, AC = area({circle,3}),
  6.0 = area({triangle,3,4,5}),
  ok.
enclose_test() ->
  {rectangle,3,4} = enclose({rectangle,3,4}),
  {rectangle,6,6} = enclose({circle,3}),
  {rectangle,5,2.4} = enclose({triangle,3,4,5}),
  ok.
bits2_test() ->
  3 = bits2(7),
  1 = bits2(8),
  7 = bits2(127),
  1 = bits2(256),
  ok.
bits_test() ->
  3 = bits(7),
  1 = bits(8),
  7 = bits(127),
  1 = bits(256),
  ok.
