-module(utils).
-export([printList/1, clock/2]). 

% Prints a list of strings or atoms
printList([]) -> empty_list;
printList([H|[]]) -> io:format("~p~n",[H]);
printList([H|T]) ->
    io:format("~p, ",[H]),
    printList(T).

% Creates a clock that sends the tuple {tick, Cycle} with cycle being equal to the number of passed cycles
% every Duration millisecond to the process ReceiverPid
clock(ReceiverPid, Duration) -> clock(ReceiverPid, Duration, 1).
clock(ReceiverPid, Duration, Cycle) ->
    timer:wait(Duration),
    ReceiverPid ! {tick, Cycle},
    clock(ReceiverPid, Duration, Cycle + 1).