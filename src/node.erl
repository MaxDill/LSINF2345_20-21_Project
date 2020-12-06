-module(node).
-import(utils, [printList/1, select_view/5, permute/1, move_oldest/2, select_peer/2, head/2, increase_age/1]).
-export([init/5, getNeigs/2]). 

getNeigs(BootServerPid, NodeId) ->
  BootServerPid ! { getNeighborgs, { erlang:self(), NodeId } },
  receive
    { getNeighborgsOk, Neigs } ->
        io:format("Node ~p successfuly got its neighborgs :", [NodeId]),
        printList(Neigs),
        Neigs
  end.

listen(active, MyId, View, Pull, C, Peer_Selection, H, S) ->
    receive
        {doActivePush} ->
            {P_id, P_pid} = select_peer(View, Peer_Selection),
            Buffer = [{MyId, self(), 0}],
            Permuted_view = permute(View),
            Moved_oldest_view = move_oldest(Permuted_view, H),
            Appended_buffer = Buffer ++ head(Moved_oldest_view, C),
            P_pid ! {doPassive, {{MyId, self()}, Appended_buffer}},
            if Pull ->
                listen(active, MyId, Moved_oldest_view, Pull,  C, Peer_Selection, H, S);
            true ->
                Inc_view = increase_age(Moved_oldest_view),
                listen(active, MyId, Inc_view, Pull,  C, Peer_Selection, H, S)
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
            listen(active, MyId, Inc_view, Pull,  C, Peer_Selection, H, S);

        {doActivePull, {{Received_id, Received_pid}, Received_buffer}} ->
            Selected_view = select_view(View, C, H, S, Received_buffer),
            Inc_view = increase_age(Selected_view),
            listen(active, MyId, Inc_view, Pull,  C, Peer_Selection, H, S);
        deactivate -> listen(inactive, MyId, View, Pull, C, Peer_Selection, H, S);
        kill -> ok
    end;
listen(inactive, MyId, View, Pull, C, Peer_Selection, H, S) ->
    receive
        kill -> ok;
        {setView, NewView} -> 
            io:format("View of node ~p has been set to : ", [MyId]), utils:printList(NewView),
            listen(inactive, MyId, NewView, Pull, C, Peer_Selection, H, S);
        activate -> listen(active, MyId, View, Pull, C, Peer_Selection, H, S);
        _ -> 
            io:format("Unstarted or faulty node ~p discarded a message~n", [MyId]),
            listen(inactive, MyId, View, Pull, C, Peer_Selection, H, S)
    end.

init(Pull, C, Peer_Selection, H, S) ->
    receive {setId, Id} ->
        io:format("~p got assigned the id ~p~n", [self(), Id]),
        listen(inactive, Id, [], Pull, C, Peer_Selection, H, S)
    end.