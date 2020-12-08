% A server that manages nodes, it is structured as a double linked list.

-module(server). 
-export([listen/2]). 
-import(utils, [printList/1, permute/1]).

% Look for neighborgs for a specific Target ({Id, Pid})
% server:lookForNeighborgs([{0, <0.84.0>}, {1, <0.85.0>}, {2, <0.86.0>}, {3, <0.87.0>}], {2, <0.86.0>}).
lookForNeighborgs(Network, Target) -> lookForNeighborgs(no_previous, Network, Target).
lookForNeighborgs(_, [H|T], Target) when H =/= Target -> lookForNeighborgs(H, T, Target); % Element not found, continue
lookForNeighborgs(_, [], _) -> unknown_node; % Node not in the network
lookForNeighborgs(no_previous, [H|[H2|_]], Target) when H =:= Target -> [H2]; % Element found at the first position and there is an element after it
lookForNeighborgs(no_previous, [H|[]], Target) when H =:= Target -> []; % Element found at the first position and there is no element after it
lookForNeighborgs(Previous, [H|[]], Target) when H =:= Target -> [Previous]; % Element found at the last position
lookForNeighborgs(Previous, [H1|[H2|_]], Target) when H1 =:= Target -> [Previous,H2]. % Element find in the middle of the list

% Initialize the view of every node in the network to its neighborgs
initAllView(Network) -> initAllView(Network, Network).
initAllView([], _) -> initAllView_done;
initAllView([{Id, Pid}|T], Network) ->
   Pid ! {setView, add_age_to_view(lookForNeighborgs(Network, {Id, Pid}))},
   initAllView(T, Network).

% Initialize the view of every node in the network after the index Idx
initAllViewAfter(Network, Idx) -> initAllViewAfter(Network, Network, Idx, 0).
initAllViewAfter([], _, _, _) -> initAllViewAfter_done;
initAllViewAfter([_|T], Network, Idx, CurIdx) when Idx >= CurIdx -> initAllViewAfter(T, Network, Idx, CurIdx+1);
initAllViewAfter([{Id, Pid}|T], Network, Idx, CurIdx) when Idx < CurIdx ->
   Pid ! {setView, add_age_to_view(lookForNeighborgs(Network, {Id, Pid}))},
   initAllViewAfter(T, Network, Idx, CurIdx+1).

% Apply the funtion FunToApply to all element of a list
applyAll([], _) -> applyAll_done;
applyAll([H|T], FunToApply) -> 
   FunToApply(H),
   applyAll(T, FunToApply).

applyAllAfter(Network, FunToApply, Idx) -> applyAllAfter(Network, FunToApply, Idx, 0).
applyAllAfter([], _, _, _) -> applyAllAfter_done;
applyAllAfter([_|T], FunToApply, Idx, CurIdx) when Idx >= CurIdx -> applyAllAfter(T, FunToApply, Idx, CurIdx+1);
applyAllAfter([H|T], FunToApply, Idx, CurIdx) when Idx < CurIdx ->
   FunToApply(H),
   applyAllAfter(T, FunToApply, Idx, CurIdx+1).

% Add an age of 0 to a view of type [{NodeId, NodePid},...] such that it returns [{NodeId, NodePid, 0},...]
add_age_to_view(View) ->  add_age_to_view(View, []).
add_age_to_view([], Acc) -> Acc;
add_age_to_view([{Id, Pid}|T], Acc) -> add_age_to_view(T, Acc ++ [{Id, Pid, 0}]).

listen(NodeID, Network) ->
   %io:format("Bootstrap server is listening...~n", []),
   receive

      {add, Pid} -> 
         Pid ! {setId, NodeID},
         listen(NodeID + 1, Network ++ [{NodeID, Pid}]);

      {initializeView, all} ->
         initAllView(Network),
         listen(NodeID, Network);
      {initializeView, Idx} ->
         initAllViewAfter(Network, Idx),
         listen(NodeID, Network);

      {activate, all} ->
         applyAll(Network, fun({_, Pid}) -> Pid ! activate end),
         listen(NodeID, Network);
      {activate, Idx} ->
         applyAllAfter(Network, fun({_, Pid}) -> Pid ! activate end, Idx),
         listen(NodeID, Network);

      {doActive, all} ->
         applyAll(Network, fun({_, Pid}) -> Pid ! {doActivePush} end),
         listen(NodeID, Network);

      {crash, NbToCrash} ->
         Permuted_network = permute(Network),
         Nodes_to_crash = lists:sublist(Permuted_network, NbToCrash), % Select random nodes to crash
         Alive_nodes = lists:sublist(Permuted_network, NbToCrash+1, length(Permuted_network)),
         applyAll(Nodes_to_crash, fun({_, Pid}) -> Pid ! deactivate end),
         listen_crash(NodeID, Network, Nodes_to_crash, Alive_nodes);

      print ->
         printList(Network),
         listen(NodeID, Network);

      stop -> server_stopped_ok

   end.

listen_crash(NodeID, Network, Crashed_nodes, Alive_nodes) ->
   %io:format("Bootstrap server is listening...~n", []),
   receive

      recover ->
         case Alive_nodes of [H|_] -> 
            applyAll(Crashed_nodes, fun({_, Pid}) -> Pid ! {setView, add_age_to_view([H])} end),
            applyAll(Crashed_nodes, fun({_, Pid}) -> Pid ! activate end),
            listen(NodeID, Network);
         [] ->
            io:format("Cannot recover : all nodes in the network are dead", []),
            server_stopped_error
         end;

      {doActive, all} ->
         applyAll(Alive_nodes, fun({_, Pid}) -> Pid ! {doActivePush} end),
         listen_crash(NodeID, Network, Crashed_nodes, Alive_nodes);

      stop -> server_stopped_ok
   end.