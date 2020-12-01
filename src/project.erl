- module(project).
- import(server, [listen/2]).
- import(node, [join/2, getNeigs/3, listen/0]).
- export([launch/1]).

findPidFromId(_, []) -> notfound;
findPidFromId(LookFor, [{Id, Pid}|_]) when LookFor =:= Id -> Pid;
findPidFromId(LookFor, [{Id, _}|T]) when LookFor =/= Id -> findPidFromId(LookFor, T).

makeNet(N, BootServerPid) -> makeNet(N, BootServerPid, [], 0).

makeNet(N, BootServerPid, Net, Counter) ->
  NodePid = spawn(node, listen, [[]]), % Create a new node with an empty view
  NodeId = node:join(BootServerPid), % Add it to the network
  Node = { NodeId, NodePid },
  if
    N =/= Counter + 1 ->
      makeNet(N, BootServerPid, Net ++ [ Node ], Counter + 1);
    N =:= Counter + 1 ->
      Net ++ [ Node ]
  end.

launch(N) ->
  % Creates server with an empty tree
  BootServerPid = spawn(server, listen, [ 0, [] ]),
  io:format("Server pid is ~p~n", [BootServerPid]),
  makeNet(N, BootServerPid).
