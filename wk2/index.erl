-module(index).
%-export([get_file_contents/1,show_file_contents/1]).
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

main() ->
  index('gettysburg-address.txt').

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
get_tokens([]) -> [];
get_tokens([X|Xs]) ->
  [string:tokens(X, " -.,\\\t()\"") | get_tokens(Xs)].

% remove words less that length 3 from list of tokens
remove_short_words(Xs) -> lists:filter(fun(X)->length(X)>3 end, Xs).

% remove duplicates
nub([]) -> [];
nub([X|Xs]) ->
  [X|nub(lists:filter(fun(Y)->X=/=Y end,Xs))].

% index contents of a text file
index_text_file(File) ->
  Lines=get_file_contents(File),
  LinesWithNumbers=line_number(Lines),
  Words=nub(remove_short_words(lists:sort(lists:concat(get_tokens(Lines))))),
  tokens_and_linenumbers(Words, LinesWithNumbers).

% transform a list of Tokens and LinesWithNumbers to list of tuples {Token,LineNumbers}
% where LineNumbers is an array of linenumbers a given token appears in
% return [{T1,[{Na,Nb}...]},{T2,[{Nc,Nd}...]},...]
tokens_and_linenumbers([],_Ls) -> [];
tokens_and_linenumbers([W|Ws],Ls) ->
  {A,_B}=lists:unzip(lists:filter(fun({_N,L}) -> lists:member(W,hd(get_tokens([L]))) end, Ls)),
  [{W,A} | tokens_and_linenumbers(Ws,Ls)].

run_tests() ->
  [{3,5},{7,7},{11,13}]=numbers_to_ranges([3,4,5,7,11,12,13]),
  ok.
