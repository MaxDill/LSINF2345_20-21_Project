-module(logging).
-import(utils, [printList/1]).
-export([listen/1]).

% spawn(logging, listen, [[]]).
% <0.82.0> ! {log, {1, 3, [{0, <0.84.0>, 1}, {1, <0.85.0>, 8}, {2, <0.86.0>, 3}, {3, <0.87.0>, 5}]}}.
% <0.82.0> ! {write, "logs.txt"}.

listen(Logs) ->
    receive
        {log, {CycleNo, Id, View}} ->
            listen(Logs ++ [{CycleNo, Id, View}]);
        {write, FileName} ->
            FormattedLogs = formatLogs(Logs),
            file:write_file(FileName, [lists:flatten(FormattedLogs)]),
            listen(Logs);
        stop -> ok
    end.

formatLogs(Logs) -> formatLogs(Logs, []).
formatLogs([], Acc) -> Acc;
formatLogs([{CycleNo, Id, View}|T], Acc) ->
    FormattedView = formatView(View),
    formatLogs(T, Acc ++ io_lib:format("~p - ~p : ~s.~n", [CycleNo, Id, FormattedView])).

formatView(View) -> formatView(View, []).
formatView([], Acc) -> lists:flatten(Acc);
formatView([{Id, Pid, Age}|T], Acc) -> formatView(T, Acc ++ io_lib:format(" {~p, ~p, ~p} ", [Id, Pid, Age])).
