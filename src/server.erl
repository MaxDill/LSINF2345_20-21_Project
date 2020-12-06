% A server that manages nodes, it is structured as a double linked list.

-module(server). 
-export([listen/2]). 
-import(utils, [printList/1]).

lookForNeighborgs(Network, Target) -> lookForNeighborgs(no_previous, Network, Target).
lookForNeighborgs(_, [H|T], Target) when H =/= Target -> lookForNeighborgs(H, T, Target); % Element not found, continue
lookForNeighborgs(_, [], _) -> unknown_node; % Node not in the network
lookForNeighborgs(no_previous, [H|[H2|_]], Target) when H =:= Target -> [H2]; % Element found at the first position and there is an element after it
lookForNeighborgs(no_previous, [H|[]], Target) when H =:= Target -> []; % Element found at the first position and there is no element after it
lookForNeighborgs(Previous, [H|[]], Target) when H =:= Target -> [Previous]; % Element found at the last position
lookForNeighborgs(Previous, [H1|[H2|_]], Target) when H1 =:= Target -> [Previous,H2]. % Element find in the middle of the list

initAllView(Network) -> initAllView(Network, Network).
initAllView([], _) -> initAllView_done;
initAllView([{Id, Pid}|T], Network) ->
   io:format("~p~n", [Id]),
   Pid ! {setView, add_age_to_view(lookForNeighborgs(Network, {Id, Pid}))},
   initAllView(T, Network).

% Add an age of 0 to a view of type [{NodeId, NodePid},...] such that it returns [{NodeId, NodePid, 0},...]
add_age_to_view(View) ->  add_age_to_view(View, []).
add_age_to_view([], Acc) -> Acc;
add_age_to_view([{Id, Pid}|T], Acc) -> add_age_to_view(T, Acc ++ [{Id, Pid, 0}]).

listen(NodeID, Network) ->
   io:format("Bootstrap server is listening...~n", []),
   receive
      stop ->
        io:format("Bootstrap server stopped~n", []),
        ok;
      {add, Pid} -> 
         Pid ! {setId, NodeID},
         listen(NodeID + 1, Network ++ [{NodeID, Pid}]);
      {initializeView, NodeID} -> % Inititalize the view a specific node
         todo;
      {initializeView, all} -> % Initialize the view of all nodes
         initAllView(Network),
         listen(NodeID, Network);
      print ->
         printList(Network),
         listen(NodeID, Network)
   end.
