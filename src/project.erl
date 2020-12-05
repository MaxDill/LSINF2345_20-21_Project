- module(project).
- import(server, [listen/2]).
- import(node, [join/2, getNeigs/3, listen/0]).
- export([launch/6]).

findPidFromId(_, []) -> notfound;
findPidFromId(LookFor, [{Id, Pid}|_]) when LookFor =:= Id -> Pid;
findPidFromId(LookFor, [{Id, _}|T]) when LookFor =/= Id -> findPidFromId(LookFor, T).

makeNet(N, BootServerPid, Pull, C, Peer_Selection, H, S) -> makeNet(N, BootServerPid, [], 0, Pull, C, Peer_Selection, H, S).

makeNet(N, BootServerPid, Net, Counter, Pull, C, Peer_Selection, H, S) ->
  NodePid = spawn(node, init, [Pull, C, Peer_Selection, H, S]), % Create a new node with an empty view
  NodeId = node:join(BootServerPid), % Add it to the network
  NodePid ! {setId, NodeId},
  Node = { NodeId, NodePid },
  if
    N =/= Counter + 1 ->
      makeNet(N, BootServerPid, Net ++ [ Node ], Counter + 1, Pull, C, Peer_Selection, H, S);
    N =:= Counter + 1 ->
      Net ++ [ Node ]
  end.

% Launches a scenario for the gossip algorithm
% Paramaters are : 
% N, the number of nodes in the network,
% Timer, the time in milliseconds of each cycle,
% Pull = [true,false], defining if we need to do only push (false) or push-pull (true),
% C, the maximal size of View a node can have,
% View_selection = [blind, healer, swapper], the view selection policy,
% Peer_selection = [rand, tail], the peer selection policy
%
% Example : project:launch(10, 3000, true, 4, blind, rand).
launch(N, Timer, Pull, C, View_selection, Peer_Selection) ->
  BootServerPid = spawn(server, listen, [ 0, [] ]),
  io:format("Server pid is ~p~n", [BootServerPid]),
  Network = launch(N, Timer, BootServerPid, Pull, C, View_selection, Peer_Selection),
  io:format("Network successfuly created : ", []),
  utils:printList(Network).
  

launch(N, Timer, BootServerPid, Pull, C, View_selection, Peer_Selection) ->
  if View_selection =:= blind ->
    makeNet(N, BootServerPid, Pull, C, Peer_Selection, 0, 0);
  View_selection =:= healer ->
    makeNet(N, BootServerPid, Pull, C, Peer_Selection, C/2, 0);
  View_selection =:= swapper ->
    makeNet(N, BootServerPid, Pull, C, Peer_Selection, 0, C/2);
  true ->
    throw("Invalid View_selection parameter")
  end.
