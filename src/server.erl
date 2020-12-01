% A server that manages nodes, it is structured as a double linked list.

-module(server). 
-export([listen/2]). 
-import(utils, [printList/1]).

lookForNeighborgs(_, [H|T], Target) when H =/= Target -> lookForNeighborgs(H, T, Target); % Element not found, continue
lookForNeighborgs(_, [], _) -> unknown_node; % Node not in the network
lookForNeighborgs(no_previous, [H|[H2|_]], Target) when H =:= Target -> [H2]; % Element found at the first position and there is an element after it
lookForNeighborgs(no_previous, [H|[]], Target) when H =:= Target -> []; % Element found at the first position and there is no element after it
lookForNeighborgs(Previous, [H|[]], Target) when H =:= Target -> [Previous]; % Element found at the last position
lookForNeighborgs(Previous, [H1|[H2|_]], Target) when H1 =:= Target -> [Previous,H2]. % Element find in the middle of the list

listen(NodeID, Network) ->
   io:format("Bootstrap server is listening...~n", []),
   receive
      stop ->
        io:format("Bootstrap server stopped~n", []),
         ok;
      {add, From} -> 
         From ! {joinOk, NodeID},
         listen(NodeID + 1, [NodeID|Network]);
      {getNeighborgs, {From, Of}} -> 
         Neighborgs = lookForNeighborgs(no_previous, Network, Of),
         From ! {getNeighborgsOk, Neighborgs},
         listen(NodeID, Network);
      print ->
         printList(Network)
   end.
