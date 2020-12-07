- module(project).
- import(server, [listen/2]).
- import(node, [join/2, getNeigs/3, listen/0]).
- import(logging, [listen/1]).
- export([launch/6]).

makeNet(N, BootServerPid, Pull, C, Peer_Selection, H, S, LoggingPid) -> makeNet(N, BootServerPid, 0, Pull, C, Peer_Selection, H, S, LoggingPid).

makeNet(N, BootServerPid, Counter, Pull, C, Peer_Selection, H, S, LoggingPid) ->
  NodePid = spawn(node, init, [Pull, C, Peer_Selection, H, S, LoggingPid]), % Create a new node with an empty view
  BootServerPid ! {add, NodePid}, % Add it to the network
  if
    N =/= Counter + 1 ->
      makeNet(N, BootServerPid, Counter + 1, Pull, C, Peer_Selection, H, S, LoggingPid);
    N =:= Counter + 1 ->
      BootServerPid ! print
  end.

% Launches a network
% Paramaters are : 
% N, the number of nodes in the network,
% Timer, the time in milliseconds of each cycle,
% Pull = [true,false], defining if we need to do only push (false) or push-pull (true),
% C, the maximal size of View a node can have,
% View_selection = [blind, healer, swapper], the view selection policy,
% Peer_selection = [rand, tail], the peer selection policy
%
% Example : project:launch(10, 3000, false, 3, healer, rand).
% Example : project:launch(128, 3000, false, 7, healer, tail).
launch(N, Timer, Pull, C, View_selection, Peer_selection) ->
  BootServerPid = spawn(server, listen, [ 0, [] ]),
  LoggingPid = spawn(logging, listen, [[]]),
  io:format("Server pid is ~p~n", [BootServerPid]),
  io:format("Logging pid is ~p~n", [LoggingPid]),
  % Phase 1 : initialize 40 % of nodes, then set initial view to all the initialized nodes, then activate all the nodes
  N_40 = floor(N*0.4),
  N_20 = floor(N*0.2),
  createNetwork(N_40, BootServerPid, Pull, C, View_selection, Peer_selection, LoggingPid),
  BootServerPid ! {initializeView, all},
  BootServerPid ! {activate, all},
  % Phase 2 : for 3 times : let pass 30 cyles then add 20% of remaining
  startCycles(10, Timer, BootServerPid),
  createNetwork(N_20, BootServerPid, Pull, C, View_selection, Peer_selection, LoggingPid),
  BootServerPid ! {initializeView, N_40 - 1}, % Initialize the view for all nodes after index N_40 - 1 (= all freshly created nodes)
  BootServerPid ! {activate, N_40 - 1},

  LoggingPid ! {write, "logs.txt"},


  startCycles(30, Timer, BootServerPid),
  createNetwork(N_20, BootServerPid, Pull, C, View_selection, Peer_selection, LoggingPid),
  BootServerPid ! {initializeView, N_40 + N_20 - 1},
  BootServerPid ! {activate, N_40 + N_20 - 1},

  startCycles(30, Timer, BootServerPid),
  createNetwork(N_20, BootServerPid, Pull, C, View_selection, Peer_selection, LoggingPid),
  BootServerPid ! {initializeView, N_40 + N_20 + N_20 - 1},
  BootServerPid ! {activate, N_40 + N_20 + N_20 - 1},
  startCycles(30, Timer, BootServerPid),

  done.

createNetwork(N, BootServerPid, Pull, C, View_selection, Peer_Selection, LoggingPid) ->
  if View_selection =:= blind ->
    makeNet(N, BootServerPid, Pull, C, Peer_Selection, 0, 0, LoggingPid);
  View_selection =:= healer ->
    makeNet(N, BootServerPid, Pull, C, Peer_Selection, ceil(C/2), floor(C/2), LoggingPid);
  View_selection =:= swapper ->
    makeNet(N, BootServerPid, Pull, C, Peer_Selection, floor(C/2), ceil(C/2), LoggingPid);
  true ->
    throw("Invalid View_selection parameter")
  end.

startCycles(0, _, _) -> startCycles_ok;
startCycles(CurCycle, Timer, BootServerPid) ->
  io:format("Running cycle : ~p", [CurCycle]),
  io:format("~n", []),
  BootServerPid ! {doActive, all},
  timer:sleep(Timer),
  startCycles(CurCycle - 1, Timer, BootServerPid).