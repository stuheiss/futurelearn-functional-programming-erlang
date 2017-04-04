-module(textformat).
-compile(export_all).

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

format_text_file(Filename, Length) ->
  Lines=get_file_contents(Filename),
  Words=lines_to_words(Lines),
  %L=lists:concat(format_text(Words,Length,0,[],[])),
  L=format_text(Words,Length,0,[],[]),
  %show_file_contents(L).
  L.

format_text([], _Length, _Count, [], Acc) -> Acc;
format_text([], _Length, _Count, Partial, Acc) -> Acc ++ lists:join(" ",Partial);
format_text([X|Xs], Length, Count, Partial, Acc) when length(X)+Count >= Length ->
  %io:format("1 ~p ~p ~p ~p~n",[X,Length,Count,Partial]),
  format_text([X|Xs], Length, 0, [], Acc ++ lists:join(" ",Partial));
format_text([X|Xs], Length, Count, Partial, Acc) ->
  %io:format("2 ~p ~p ~p ~p~n",[X,Length,Count,Partial]),
  format_text(Xs, Length, string:len(X)+Count, Partial ++ [X], Acc).

lines_to_words([]) -> [];
lines_to_words([X|Xs]) -> tokenize(X) ++ lines_to_words(Xs).
tokenize(S) ->
    {ok,REDivider} = re:compile("\\W+"), % \W in regex lingo is "non word characters".
    re:split(S,REDivider,[{return,list}]).

main() ->
  format_text_file('gettysburg-address.txt', 30).
