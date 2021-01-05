- module(project).
- import(server, [listen/2]).
- import(node, [join/2, getNeigs/3, listen/0]).
- import(logging, [listen/1]).
- export([launch/8, launch_scenario_1/0, launch_scenario_2/0, unit_test/0]).

launch_scenario_1() ->
  project:launch(128, 180, 3000, true, 7, healer, tail, "scenario1_logs.txt").

launch_scenario_2() ->
  project:launch(128, 180, 3000, true, 7, swapper, tail, "scenario2_logs.txt").

unit_test() ->
  BootServerPid = spawn(server, listen, [ 0, []]),
  NodePid1 = spawn(node, init, [true, 5, swapper, 0, 0, undefined]),
  NodePid2 = spawn(node, init, [true, 5, swapper, 0, 0, undefined]),
  NodePid3 = spawn(node, init, [true, 5, swapper, 0, 0, undefined]),
  io:format("Network should be empty :", []),
  BootServerPid ! print,
  BootServerPid ! {add, NodePid1},
  io:format("Network should have one element with pid ~p : ", [NodePid1]),
  BootServerPid ! print,
  BootServerPid ! {add, NodePid2},
  io:format("Network should have 2 elements with pid ~p and ~p : ", [NodePid1, NodePid2]),
  BootServerPid ! print,
  BootServerPid ! {add, NodePid3},
  io:format("Network should have 3 elements with pid ~p and ~p and ~p : ", [NodePid1, NodePid2, NodePid3]),
  BootServerPid ! print.


% Launches a scenario (based on what is described in the instructions
% Paramaters are :
% N, the number of nodes in the network,
% NbOfCycle defines the total number of cycles for the scenario
% Timer, the time in milliseconds of each cycle,
% Pull = [true,false], defining if we need to do only push (false) or push-pull (true),
% C, the maximal size of View a node can have,
% View_selection = [blind, healer, swapper], the view selection policy,
% Peer_selection = [rand, tail], the peer selection policy,
% LogsOutputFileName is string that defines the name of the logs file
%
% Example : project:launch(128, 180, 3000, false, 7, healer, tail, "logs.txt").
launch(N, NbOfCycles, Timer, Pull, C, View_selection, Peer_selection, LogsOutputFileName) ->
  BootServerPid = spawn(server, listen, [ 0, []]),
  LoggingPid = spawn(logging, listen, [[]]),
  io:format("Server pid is ~p~n", [BootServerPid]),
  io:format("Logging pid is ~p~n", [LoggingPid]),
  % We cut for the different phases
  N_40 = trunc(N*0.4), % 40% of N
  N_20 = trunc(N*0.2), % 20 % of N
  N_60 = trunc(N*0.6), % 60 % of N
  Nb_Cycles_20 = trunc(NbOfCycles/6), % 20% of NbOfCycles
  % Phase 1 : initialize 40 % of nodes, then set initial view to all the initialized nodes, then activate all the nodes
  addToNetwork(N_40, BootServerPid, Pull, C, View_selection, Peer_selection, LoggingPid),
  BootServerPid ! {initializeView, all},
  BootServerPid ! {activate, all},
  % Phase 2 : for 3 times : let pass 30 cyles then add 20% of remaining
  startCycles(Nb_Cycles_20, Timer, BootServerPid),
  addToNetwork(N_20, BootServerPid, Pull, C, View_selection, Peer_selection, LoggingPid),
  BootServerPid ! {initializeView, N_40 - 1}, % Initialize the view for all nodes after index N_40 - 1 (= all freshly created nodes)
  BootServerPid ! {activate, N_40 - 1},

  startCycles(Nb_Cycles_20, Timer, BootServerPid),
  addToNetwork(N_20, BootServerPid, Pull, C, View_selection, Peer_selection, LoggingPid),
  BootServerPid ! {initializeView, N_40 + N_20 - 1},
  BootServerPid ! {activate, N_40 + N_20 - 1},

  startCycles(Nb_Cycles_20, Timer, BootServerPid),
  addToNetwork(N_20, BootServerPid, Pull, C, View_selection, Peer_selection, LoggingPid),
  BootServerPid ! {initializeView, N_40 + N_20 + N_20 - 1},
  BootServerPid ! {activate, N_40 + N_20 + N_20 - 1},
  startCycles(Nb_Cycles_20, Timer, BootServerPid),

  % Phase 3, 60% of the nodes crash (randomly chosen)
  BootServerPid ! {crash, N_60},
  startCycles(Nb_Cycles_20, Timer, BootServerPid),

  % Phase 4 : the crashed nodes recover, their views are a random node in the alive nodes
  BootServerPid ! recover,
  startCycles(Nb_Cycles_20, Timer, BootServerPid),

  % Write all the logs in the file logs.txt
  LoggingPid ! {write, LogsOutputFileName},

  ok.

% Add N nodes with specif gossip algorithm parmeters to the network of BootServerPid
addToNetwork(N, BootServerPid, Pull, C, View_selection, Peer_Selection, LoggingPid) ->
  if View_selection =:= blind ->
    makeNet(N, BootServerPid, Pull, C, Peer_Selection, 0, 0, LoggingPid);
  View_selection =:= healer ->
    makeNet(N, BootServerPid, Pull, C, Peer_Selection, trunc(C/2)+1, trunc(C/2), LoggingPid);
  View_selection =:= swapper ->
    makeNet(N, BootServerPid, Pull, C, Peer_Selection, trunc(C/2), trunc(C/2)+1, LoggingPid);
  true ->
    throw("Invalid View_selection parameter")
  end.

% Start mutlipe cylces with CurCycle number of cycles whith each taking Timer time.
% A cycle is when all (alive) nodes in the network excecute their active thread.
startCycles(0, _, _) -> startCycles_ok;
startCycles(CurCycle, Timer, BootServerPid) ->
  io:format("Running cycle : ~p", [CurCycle]),
  io:format("~n", []),
  BootServerPid ! {doActive, all},
  timer:sleep(Timer),
  startCycles(CurCycle - 1, Timer, BootServerPid).

% Called by addToNet to add nodes to the network (see addToNet for more information)
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
