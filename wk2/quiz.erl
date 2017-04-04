-module(quiz).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

nand(A,B) -> not(A and B).

f1(A,B) -> not(A) or not(B).
f2(A,B) -> not(A and B).
f3(A,B) -> not(A andalso B).
f4(A,B) -> (not(A) and B) or (not(B) and A).

merge([],Ys) -> Ys;
merge(Xs,[]) -> Xs;
merge([X|Xs],[Y|Ys]) when X<Y ->
    [ X | merge(Xs,[Y|Ys]) ];
merge([X|Xs],[Y|Ys]) when X>Y ->
    [ Y | merge([X|Xs],Ys) ];
merge([X|Xs],[_Y|Ys]) ->
    [ X | merge(Xs,Ys) ].

foo(_,[])              -> [];
foo(Y,[X|_]) when X==Y -> [X];
foo(Y,[X|Xs])          -> [X | foo(Y,Xs) ].

bar(N, [N]) ->
    [];
bar(_N, [Y]) ->
     [Y];
bar(N, [Y|Ys]) when N =/= Y ->
    [Y|bar (N, Ys)];
bar(N, [_Y|Ys]) ->
    bar(N,Ys).

baz([])     -> [];
baz([X|Xs]) -> [X | baz(zab(X,Xs))].

zab(_N,[])     -> [];
zab(N,[N|Xs]) -> zab(N,Xs);
zab(N,[X|Xs]) -> [X | zab(N,Xs)].

zot(_A) -> true.
zot(_A,_B) -> false.

check(List) ->
  case List of
    [] -> empty_list;
    [1|_] -> head_check;
    [_|[2,3]] -> tail_check
  end.

check_test() ->
  ?assertEqual(empty_list, check([])),
  ?assertEqual(head_check, check([1])),
  ?assertEqual(tail_check, check([1,2,3])),
  ok.

baz_test() ->
  ?assertEqual([3,2,1], baz([3,2,2,3,2,1,2,3])),
  ok.

bar_test() ->
  ?assertEqual([4,1], bar(0,[4,0,1])),
  ?assertEqual([4,1,2], bar(0,[4,0,1,0,2])),
  ok.

foo_test() ->
  ?assertEqual([4,0], foo(0,[4,0,1])).

merge_test() ->
  ?assertEqual(6, length(merge([1,2,3],[4,5,6]))),
  ?assertEqual(3, length(merge([1,1,1],[1,1,1]))),
  ?assertEqual([1,2,3,4,5,6], merge([1,2,3],[4,5,6])),
  ?assertEqual([1,2,3,4,5,6], merge([4,5,6],[1,2,3])),
  ?assertEqual([1,2,3], merge([1,2,3],[1,2,3])),
  ?assertEqual([1,2,3,4], merge([1,2,3],[2,3,4])),
  ?assertEqual([1,2,3,4,5,6], merge([1,3,5],[2,4,6])),
  ?assertEqual([1,2,3,4,5,6], merge([1,3,5],[1,2,4,6])),
  ?assertEqual([1,2,3,4,5,6,7,7,7], merge([1,2,3],[4,5,6,7,7,7])),
  true.


f1_test() ->
  ?assertEqual(nand(true,true), f1(true,true)),
  ?assertEqual(nand(true,false), f1(true,false)),
  ?assertEqual(nand(false,true), f1(false,true)),
  ?assertEqual(nand(false,false), f1(false,false)).
f2_test() ->
  ?assertEqual(nand(true,true), f2(true,true)),
  ?assertEqual(nand(true,false), f2(true,false)),
  ?assertEqual(nand(false,true), f2(false,true)),
  ?assertEqual(nand(false,false), f2(false,false)).
f3_test() ->
  ?assertEqual(nand(true,true), f3(true,true)),
  ?assertEqual(nand(true,false), f3(true,false)),
  ?assertEqual(nand(false,true), f3(false,true)),
  ?assertEqual(nand(false,false), f3(false,false)).
f4_test_skip() ->
  ?assertEqual(nand(true,true), f4(true,true)),
  ?assertEqual(nand(true,false), f4(true,false)),
  ?assertEqual(nand(false,true), f4(false,true)),
  ?assertEqual(nand(false,false), f4(false,false)).
