-module(node).
-import(utils, [printList/1]).
-export([init/5]). 

% Function that is used to select a new view depending of the view selection policy
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

% Active thread
active(Pull_policy, Peer_selection, View_selection, H, S, View) ->
    timer:sleep(3000),
    P = select_peer(Peer_selection, View),
    Buffer = [{erlang:self(), 0}],
    Permuted_view = permute(View),
    Moved_oldest_view = move_oldest(Permuted_view),
    Appended_buffer = append(Buffer, Moved_oldest_view),
    P ! {erlang:self(), Appended_buffer},
    done.

% Passive thread
passive() ->
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
        io:format("Node ~s successfuly retrieved neighborgs of node ~s : ", [erlang:self(), NodeId]),
        printList(Neigs),
        Neigs
  end.

% Initialise a new node with a different parameters, a node is defined as a dispatcher, an active and a passive thread
init(Pull_policy, Peer_selection, View_selection, H, S) -> 
    InitView = todo, %TODO
    ActivePID = spawn(fun() -> active(Pull_policy, Peer_selection, View_selection, H, S, InitView) end),
    PassivePID = spawn(fun() -> passive(Pull_policy, Peer_selection, View_selection, H, S, InitView) end),
    PassivePID.