-module(node).
-import(utils, [printList/1, select_view/5, permute/1, move_oldest/3, select_peer/2, head/2, increase_age/1]).
-export([init/6]). 

% Active node, actual implementation of the gossip algorithm is right here
listen(active, MyId, View, Pull, C, Peer_Selection, H, S, CycleNo, LoggingPid) ->
    %io:format("Node ~p is listening at cycle ~p~n", [MyId, CycleNo]),
    receive
        {doActivePush} ->
            LoggingPid ! {log, {CycleNo, MyId, View}},
            {P_id, P_pid, _} = select_peer(View, Peer_Selection),
            %io:format("Node ~p selected node ~p~n", [MyId, P_id]),
            Buffer = [{MyId, self(), 0}],
            Permuted_view = permute(View),
            Moved_oldest_view = move_oldest(Permuted_view, H, C),
            Appended_buffer = Buffer ++ head(Moved_oldest_view, C),
            P_pid ! {doPassive, {{MyId, self()}, Appended_buffer}},
            if Pull ->
                listen(active, MyId, Moved_oldest_view, Pull,  C, Peer_Selection, H, S, CycleNo, LoggingPid);
            true ->
                Inc_view = increase_age(Moved_oldest_view),
                listen(active, MyId, Inc_view, Pull,  C, Peer_Selection, H, S, CycleNo + 1, LoggingPid)
            end;

        {doPassive, {{Received_id, Received_pid}, Received_buffer}} ->
            if Pull ->
                Buffer = [{MyId, self(), 0}],
                Permuted_view = permute(View),
                Moved_oldest_view = move_oldest(Permuted_view, H, C),
                Appended_buffer = Buffer ++ head(Moved_oldest_view, C),
                Received_pid ! {doActivePull, {{MyId, self()}, Appended_buffer}},

                Selected_view = select_view(View, C, H, S, Received_buffer),
                Inc_view = increase_age(Selected_view),
                listen(active, MyId, Inc_view, Pull,  C, Peer_Selection, H, S, CycleNo, LoggingPid);
            true -> 
                Selected_view = select_view(View, C, H, S, Received_buffer),
                Inc_view = increase_age(Selected_view),
                listen(active, MyId, Inc_view, Pull,  C, Peer_Selection, H, S, CycleNo, LoggingPid)
            end;

        {doActivePull, {{Received_id, Received_pid}, Received_buffer}} ->
            Selected_view = select_view(View, C, H, S, Received_buffer),
            Inc_view = increase_age(Selected_view),
            listen(active, MyId, Inc_view, Pull,  C, Peer_Selection, H, S, CycleNo + 1, LoggingPid);
        deactivate -> 
            io:format("Node ~p deactivated or crashed~n", [MyId]),
            listen(inactive, MyId, View, Pull, C, Peer_Selection, H, S, CycleNo, LoggingPid);
        kill -> ok
    end;

% Listen for the inactive state, a node in this state won't accept any message related to the gossip algorithm and can
% be concider inactive or crashed
% In this state, the view of the node can be changed ({setView, NewView}) or the node can be reactivate or put back to live (activate)
listen(inactive, MyId, View, Pull, C, Peer_Selection, H, S, CycleNo, LoggingPid) ->
    receive
        kill -> ok;
        {setView, NewView} -> 
            %io:format("View of node ~p has been set to : ", [MyId]), utils:printList(NewView),
            listen(inactive, MyId, NewView, Pull, C, Peer_Selection, H, S, CycleNo, LoggingPid);
        activate -> 
            io:format("Node ~p started or recovered~n", [MyId]),
            listen(active, MyId, View, Pull, C, Peer_Selection, H, S, CycleNo, LoggingPid);
        _ -> 
            %io:format("Unstarted or crashed node ~p discarded a message~n", [MyId]),
            listen(inactive, MyId, View, Pull, C, Peer_Selection, H, S, CycleNo, LoggingPid)
    end.

% Initialize a new node with specific gossip parameters
% The freshly initialised node will wait for its ID ({setId, Id}) and will then pass to the inactive state
init(Pull, C, Peer_Selection, H, S, LoggingPid) ->
    receive {setId, Id} ->
        io:format("~p got assigned the id ~p~n", [self(), Id]),
        listen(inactive, Id, [], Pull, C, Peer_Selection, H, S, 0, LoggingPid)
    end.