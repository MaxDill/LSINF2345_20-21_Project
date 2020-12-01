-module(utils).
-export([printList/1]). 

% Prints a list of strings or atoms
printList([]) -> empty_list;
printList([H|[]]) -> io:format("~p~n",[H]);
printList([H|T]) ->
    io:format("~p, ",[H]),
    printList(T).