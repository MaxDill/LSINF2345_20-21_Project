-module(node).
-import(utils, [printList/1, select_view/5, permute/1, move_oldest/2, select_peer/2, head/2, increase_age/1]).
-export([listen/7, join/1, getNeigs/2]). 

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
listen(MyId, View, Pull, C, Peer_Selection, H, S) -> 
    receive
        {doActivePush} ->
            {P_id, P_pid} = select_peer(View, Peer_Selection),
            Buffer = [{MyId, self(), 0}],
            Permuted_view = permute(View),
            Moved_oldest_view = move_oldest(Permuted_view, H),
            Appended_buffer = Buffer ++ head(Moved_oldest_view, C),
            P_pid ! {doPassive, {{MyId, self()}, Appended_buffer}},
            if Pull ->
                listen(MyId, Moved_oldest_view, Pull,  C, Peer_Selection, H, S);
            true ->
                Inc_view = increase_age(Moved_oldest_view),
                listen(MyId, Inc_view, Pull,  C, Peer_Selection, H, S)
            end;

        {doPassive, {{Received_id, Received_pid}, Received_buffer}} ->
            if Pull ->
                Buffer = [{MyId, self(), 0}],
                Permuted_view = permute(View),
                Moved_oldest_view = move_oldest(Permuted_view, H),
                Appended_buffer = Buffer ++ head(Moved_oldest_view, C),
                Received_pid ! {doActivePull, {{MyId, self()}, Appended_buffer}}
            end,
            Selected_view = select_view(View, C, H, S, Received_buffer),
            Inc_view = increase_age(Selected_view),
            listen(MyId, Inc_view, Pull,  C, Peer_Selection, H, S);

        {doActivePull, {{Received_id, Received_pid}, Received_buffer}} ->
            Selected_view = select_view(View, C, H, S, Received_buffer),
            Inc_view = increase_age(Selected_view),
            listen(MyId, Inc_view, Pull,  C, Peer_Selection, H, S);

        stop -> ok
    end.