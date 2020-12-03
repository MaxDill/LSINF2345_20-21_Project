-module(node).
-import(utils, [printList/1]).
-export([listen/7, join/1, getNeigs/2]). 

permute(X) ->
    [Y||{_,Y} <- lists:sort([ {rand:uniform(), N} || N <- X])].

move_oldest(View, H) ->
    todo.

select_peer(View, Peer_selection) ->
    todo.

head(View, C) ->
    todo.

increase_age(View) ->
    todo.

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
            Inc_view = increase_age(Moved_oldest_view),
            listen(MyId, Inc_view, Pull,  C, Peer_Selection, H, S);

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

select_view(View, C, H, S, Buffer) ->
    Appended_view = View ++ Buffer,
    Removed_duplicate_view = remove_duplicate(Appended_view),
    Removed_old_view = remove_old(Removed_duplicate_view, lists:min([H, (length(Removed_duplicate_view) - C)])),
    Removed_head = remove_head(Removed_old_view, lists:min([S, (length(Removed_old_view) - C)])),
    Removed_random = remove_at_random(Removed_head, (length(Removed_head) - C)),
    Removed_random.

remove_duplicate(View) ->
    todo.

remove_old(View, NbToRemove) ->
    todo.

remove_head(View, NbToRemove) ->
    todo.

remove_at_random(View, NbToRemove) ->
    todo.