-module(node).
-import(utils, [printList/1]).
-export([listen/1, join/1, getNeigs/2]). 

select_view() ->
    done.

permute(X) ->
    [Y||{_,Y} <- lists:sort([ {rand:uniform(), N} || N <- X])].

move_oldest(View) ->
    done.

select_peer(Peer_selection, View) ->
    done.

append(Buffer, Moved_oldest_view) ->
    done.

join(BootServerPid) ->
    BootServerPid ! { add, self() },
    receive
        { joinOk, NodeId } ->
            io:format("Node ~p successfuly joined the network~n", [NodeId]),
            NodeId
    end.

getNeigs(BootServerPid, NodeId) ->
  BootServerPid ! { getNeighborgs, { erlang:self(), NodeId } },
  receive
    { getNeighborgsOk, Neigs } ->
        printList(Neigs),
        Neigs
  end.

% Initialise a new node with a different parameters, a node is defined as a dispatcher, an active and a passive thread
listen(View) -> 
    receive
        doActive1 -> done;
        doActive2 -> done;
        passive -> done;
        stop -> ok
    end.