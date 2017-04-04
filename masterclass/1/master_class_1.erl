-module(master_class_1).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

% Erlang Master Class 1: Functional Programming
% https://www.youtube.com/playlist?list=PLR812eVbehlwEArT3Bv3UfcM9wR3AEZb5
% Erlang Master Class 2: Concurrent Programming
% https://www.youtube.com/playlist?list=PLR812eVbehlwq4qbqswOWH7NLKjodnTIn
% Erlang Master Class 3: OTP Behaviours and Releases
% https://www.youtube.com/playlist?list=PLR812eVbehlx6vgWGf2FLHjkksAEDmFjc

-type expr() :: {'num', integer()}
              | {'var', atom()}
              | {'add', expr(), expr()}
              | {'mul', expr(), expr()}.

% pretty print an expression
-spec print(expr()) -> string().
print({'num', N}) ->
  integer_to_list(N);
print({'var', A}) ->
  atom_to_list(A);
print({'add', E1, E2}) ->
  "(" ++ print(E1) ++ "+" ++ print(E2) ++ ")";
print({'mul', E1, E2}) ->
  "(" ++ print(E1) ++ "*" ++ print(E2) ++ ")".

-type env() :: [{atom(), integer()}].
-spec eval(env(), expr()) -> integer().
eval(_Env, {'num', N}) ->
  N;
eval(Env, {'var', A}) ->
  lookup(A, Env);
eval(Env, {'add', E1, E2}) ->
  eval(Env, E1) + eval(Env, E2);
eval(Env, {'mul', E1, E2}) ->
  eval(Env, E1) * eval(Env, E2).

-spec lookup(atom(), env()) -> integer().
lookup(A, E) ->
  case lists:keyfind(A,1,E) of
    {_K, V} -> V
  end.

% compile/execute
-type instr() :: {'push', integer()}
              |  {'fetch', atom()}
              |  {'add2'}
              |  {'mul2'}.
-type program() :: [instr()].
-type stack() :: [integer()].

-spec compile(expr()) -> program().
compile({num, N}) ->
  [{push, N}];
compile({var, A}) ->
  [{fetch, A}];
compile({add, E1, E2}) ->
  compile(E1) ++ compile(E2) ++ [{add2}];
compile({mul, E1, E2}) ->
  compile(E1) ++ compile(E2) ++ [{mul2}].

-spec run(program(), env(), stack()) -> integer().
run([{push, N} | Continue], Env, Stack) ->
  run(Continue, Env, [N|Stack]);
run([{fetch, A} | Continue], Env, Stack) ->
  run(Continue, Env, [lookup(A, Env)|Stack]);
run([{add2} | Continue], Env, [N1,N2|Stack]) ->
  run(Continue, Env, [(N1+N2)|Stack]);
run([{mul2} | Continue], Env, [N1,N2|Stack]) ->
  run(Continue, Env, [(N1*N2)|Stack]);
run([], _Env, [N]) ->
  N.

% parsing
-spec parse(string()) -> {expr(), string()}.
%parse([$(|Rest]) ->           % starts with a '('
%  {E1,Rest1} = parse(Rest),   % then an expression
%  [Op|Rest2] = Rest1,         % then an operator
%  {E2,Rest3} = parse(Rest2),  % then an expression
%  [$)|RestFinal] = Rest3,     % starts with a ')'
%  {case Op of
%     $+ -> {add,E1,E2};
%     $* -> {mul,E1,E2}
%   end,
%   RestFinal}.

% general parsing
% split a string into a tuple of all that matches a predicate and whatever is left
-spec get_while(fun((T) -> boolean()), [T]) -> {[T], [T]}.
get_while(P, [Ch|Rest]) ->
  case P(Ch) of
    true ->
      {Succeeds, Remainder} = get_while(P, Rest),
      {[Ch|Succeeds], Remainder};
    false ->
      {[], [Ch|Rest]}
  end;
get_while(_P, []) ->
  {[], []}.

% literals
parse([Ch|Rest]) when $a =< Ch andalso Ch =< $z ->
  {Succeeds, Remainder} = get_while(fun is_alpha/1, Rest),
  {{var, list_to_atom([Ch|Succeeds])}, Remainder};
% numbers
parse([Ch|Rest]) when $0 =< Ch andalso Ch =< $9 ->
  {Succeeds, Remainder} = get_while(fun is_number/1, Rest),
  {{num, list_to_atom([Ch|Succeeds])}, Remainder};
% add
parse([$+|Rest]) ->
  {{add}, Rest};
% mul
parse([$*|Rest]) ->
  {{mul}, Rest};
parse([]) ->
  {[], []}.
% todo: parens

is_alpha(Ch) -> $a =< Ch andalso Ch =< $z.
is_number(Ch) -> $0 =< Ch andalso Ch =< $9.

% simplification
zeroAdd({add, E, {num, 0}}) -> E;
zeroAdd({add, {num, 0}, E}) -> E;
zeroAdd(E) -> E.

mulOne({mul, E, {num, 1}}) -> E;
mulOne({mul, {num, 1}, E}) -> E;
mulOne(E) -> E.

mulZero({mul, _, {num, 0}}) -> {num, 0};
mulZero({mul, {num, 0}, _}) -> {num, 0};
mulZero(E) -> E.

addNums({add, {num, N1}, {num, N2}}) -> {num, N1+N2};
addNums(E) -> E.

mulNums({mul, {num, N1}, {num, N2}}) -> {num, N1*N2};
mulNums(E) -> E.

compose([]) ->
  fun(E) ->
      E end;
compose([Rule|Rules]) ->
  fun(E) ->
      (compose(Rules))(Rule(E)) end.

rules() ->
  [fun zeroAdd/1, fun mulOne/1, fun mulZero/1, fun addNums/1, fun mulNums/1].

simp(F, {add, E1, E2}) ->
  F({add, simp(F, E1), simp(F, E2)});
simp(F, {mul, E1, E2}) ->
  F({mul, simp(F, E1), simp(F, E2)});
simp(_F, E) ->
  E.

simplify(E) ->
  simp(compose(rules()), E).



% unit tests
print_test() ->
  ?assertEqual("(2+(3*4))",print({'add',{'num',2},{'mul',{'num',3},{'num',4}}})).

eval1_test() ->
  E=[],
  ?assertEqual(14,eval(E, {'add',{'num',2},{'mul',{'num',3},{'num',4}}})).

eval2_test() ->
  E=[{'a', 42}],
  ?assertEqual(54,eval(E, {'add',{'var','a'},{'mul',{'num',3},{'num',4}}})).

execute1_test() ->
  ?assertEqual(14,run(compile({'add',{'num',2},{'mul',{'num',3},{'num',4}}}), [], [])).

execute2_test() ->
  E=[{'a', 42}],
  ?assertEqual(54,run(compile({'add',{'var',a},{'mul',{'num',3},{'num',4}}}), E, [])).

simplify1_test() ->
  ?assertEqual({num,42},simplify({add,{num,42},{mul,{num,3},{num,0}}})).
simplify2_test() ->
  ?assertEqual({num,45},simplify({add,{num,42},{mul,{num,3},{num,1}}})).
simplify3_test() ->
  ?assertEqual(
     {var,b},
     simplify({add,{mul,{num,1},{var,b}},{mul,{num,0},{add,{mul,{num,2},{var,b}},{mul,{num,1},{var,b}}}}})).
simplify4_test() ->
  ?assertEqual(
     {add,{num,3},{var,v}},
     simplify({add,{add,{num,1},{num,2}},{var,v}})).
