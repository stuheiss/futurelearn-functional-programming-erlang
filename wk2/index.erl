-module(index).
-export([run_tests/0,main/0,bench/1,bench/2,index/1,get_file_contents/1,show_file_contents/1]).
-compile(export_all).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
  

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.    
     
% The aim of this exercise is to index a text file, by line number. We can think of the input being a list of text strings, and below we’ve provided an outline Erlang module that reads text files into this format, as well as a couple of example files to process.
%
% The output of the main function should be a list of entries consisting of a word and a list of the ranges of lines on which it occurs.
%
% For example, the entry
%
% { "foo" , [{3,5},{7,7},{11,13}] }
%
% means that the word "foo" occurs on lines 3, 4, 5, 7, 11, 12 and 13 in the file.

% bench(fun()->yourfunction(args) end)
% bench(fun()->yourfunction(args) end, "string")
bench(F,Str) ->
  io:format("~s:", [Str]),
  bench(F).

% benchmark a function with no args
bench(F) ->
    statistics(runtime),
    statistics(wall_clock),

    % your code here
    Results=F(),

    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    U1 = Time1 * 1000,
    U2 = Time2 * 1000,
    io:format("Code time=~p (~p) microseconds~n",
    [U1,U2]),
    Results.

% print contents of list to console
dump_list([])->ok;
dump_list([X|Xs])->
  io:format("~p~n",[X]),
  dump_list(Xs).

main() ->
  Index=index('gettysburg-address.txt'),
  dump_list(Index).
main(File) ->
  Index=index(File),
  dump_list(Index).

main_silent() ->
  _Index=index('gettysburg-address.txt'),
  ok.
main_silent(File) ->
  _Index=index(File),
  ok.

% index a text file by line number
index(FileName) ->
  pretty_print(index_text_file(FileName)).

% pretty print a {token,linenumbers} tuple
% transform list of ints to list of ranges
% [1,2,3,5,7,8,9] => [{1,3},{5,5},{7,9}]
pretty_print([]) -> [];
pretty_print([{Str,Ls}|Xs]) ->
  [{Str,numbers_to_ranges(Ls)}|pretty_print(Xs)].

% transform list [1,2,3,5,7,8,9] => [{1,3},{5,5},{7,9}]
numbers_to_ranges(Xs) -> numbers_to_ranges(Xs,[]).
numbers_to_ranges([],Acc) -> lists:reverse(Acc);
numbers_to_ranges([X|Xs],[]) ->
  numbers_to_ranges(Xs,[{X,X}]);
numbers_to_ranges([X|Xs],[{A,Y}|Ys]) when X-Y==1 ->
  numbers_to_ranges(Xs,[{A,X}|Ys]);
numbers_to_ranges([X|Xs],Acc) ->
  numbers_to_ranges(Xs,[{X,X}|Acc]).

% transform a list of strings to a list of tuples
% left member is linenumber, right member is string
% [L1,L2,..LN] => [{1,L1},{2,L2},...{N,LN}]
line_number(Lines) -> line_number(Lines, 1).
line_number([], _N) -> [];
line_number([X|Xs], N) ->
  case X == [] of
    true -> line_number(Xs, N+1);
    false -> [{N,X} | line_number(Xs, N+1)]
  end.

% split a list of lines of text into a list of tokens
% split on ws and punctuation
tokens_from_list(Xs) -> tokens_from_list(Xs,[]).
tokens_from_list([], Acc) -> lists:concat(Acc);
tokens_from_list([X|Xs], Acc) ->
  tokens_from_list(Xs, [tokens_from_string(X) | Acc]).

% split a line of text into a list of tokens
tokens_from_string(String) ->
  string:tokens(String, " -.,\\\t()\"").

% remove words less that length 3 from list of tokens
remove_short_words(Xs) -> lists:filter(fun(X)->length(X)>3 end, Xs).

% remove duplicates
nub([]) -> [];
nub([X|Xs]) ->
  [X|nub(lists:filter(fun(Y)->X=/=Y end,Xs))].

% return sorted and pruned list of tokens from list of strings
words_from_list(Lines) ->
  nub(remove_short_words(lists:sort(tokens_from_list(Lines)))).

bench_words_from_list(Lines) ->
  bench(fun()->nub(remove_short_words(lists:sort(tokens_from_list(Lines)))) end,"words_from_list").

bench_tokens_and_linenumbers(Words, LinesWithNumbers) ->
  bench(fun()->tokens_and_linenumbers(Words, LinesWithNumbers) end,"tokens_and_linenumbers").

% index contents of a text file
index_text_file(File) ->
  Lines=get_file_contents(File),
  LinesWithNumbers=line_number(Lines),
  Words=bench_words_from_list(Lines),
  bench_tokens_and_linenumbers(Words, LinesWithNumbers).

% benchmark inedex_text_from_file
% index:bench_index_text_file('gettysburg-address.txt').
bench_index_text_file(File) ->
  Lines=get_file_contents(File),
  LinesWithNumbers=line_number(Lines),
  Tokens=tokens_from_list(Lines),
  Words=nub(remove_short_words(lists:sort(Tokens))),
  tokens_and_linenumbers(Words, LinesWithNumbers).

% transform a list of Tokens and LinesWithNumbers to list of tuples {Token,LineNumbers}
% where LineNumbers is an array of linenumbers a given token appears in
% return [{T1,[{Na,Nb}...]},{T2,[{Nc,Nd}...]},...]
tokens_and_linenumbers([],_Ls) -> [];
tokens_and_linenumbers([W|Ws],Ls) ->
  {A,_B}=lists:unzip(lists:filter(fun({_N,L}) -> lists:member(W,tokens_from_string(L)) end, Ls)),
  [{W,A} | tokens_and_linenumbers(Ws,Ls)].

run_tests() ->
  [{3,5},{7,7},{11,13}]=numbers_to_ranges([3,4,5,7,11,12,13]),
  ok.
