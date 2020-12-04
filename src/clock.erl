-module(clock).
-export([clock/2]).

% Creates a clock that sends the tuple {tick, Cycle} with cycle being equal to the number of passed cycles
% every Duration millisecond to the process ReceiverPid
clock(ReceiverPid, Duration) -> clock(ReceiverPid, Duration, 1).
clock(ReceiverPid, Duration, Cycle) ->
    timer:wait(Duration),
    ReceiverPid ! {tick, Cycle},
    clock(ReceiverPid, Duration, Cycle + 1).