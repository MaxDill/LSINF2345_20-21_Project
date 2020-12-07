-module(utils).
-export([printList/1, select_view/5, permute/1, move_oldest/3, select_peer/2, head/2, increase_age/1, remove_duplicate/1]). 

% Prints a list of strings or atoms
printList([]) -> io:format("Empty list~n",[]), empty_list;
printList([H|[]]) -> io:format("~p~n",[H]);
printList([H|T]) ->
    io:format("~p, ",[H]),
    printList(T).

% utils:permute([{1,11,8},{2,22,6},{3,33,4}]). 
permute(X) ->
    [Y||{_,Y} <- lists:sort([ {rand:uniform(), N} || N <- X])].

% utils:move_oldest([{1,11,8},{2,22,6},{3,33,4}], 2, 7). 
move_oldest(View, _, C) when length(View) < C ->
    View;
move_oldest(View, H, C) when length(View) =:= C ->
    Sorted_by_age_view = lists:keysort(3, View),
    H_oldest = lists:sublist(Sorted_by_age_view, length(Sorted_by_age_view)-H+1, length(Sorted_by_age_view)),
    H_oldest_removed_view = delete_all_in(View, H_oldest),
    H_oldest_removed_view ++ H_oldest.

delete_all_in(View, []) -> View;
delete_all_in(View, [H|T]) -> delete_all_in(lists:delete(H, View), T).

% utils:select_peer([{1,11,5},{2,22,3},{3,33,8}], tail).
select_peer([], _) ->
    throw("select_peer received an empty view");
select_peer(View, rand) ->
    lists:nth(rand:uniform(length(View)), View);
select_peer(View, tail) ->
    select_highest_age(View, undefined);
select_peer(_, _) ->
    throw("Invalid peer selection parameter").

select_highest_age([H|T], undefined)  ->
    select_highest_age(T, H);
select_highest_age([], {Highest_Id, Highest_Pid, Highest_Age}) -> 
    {Highest_Id, Highest_Pid, Highest_Age};
select_highest_age([{CurId, CurPid, CurAge}|T], {Highest_Id, Highest_Pid, Highest_Age}) ->
    if CurAge > Highest_Age ->
        select_highest_age(T, {CurId, CurPid, CurAge});
    true ->
        select_highest_age(T, {Highest_Id, Highest_Pid, Highest_Age})
    end.

% utils:head([{1,11,5},{2,22,3},{3,33,8}], 2).
head(View, C) ->
    lists:sublist(View, C).

% utils:increase_age([{1,11,5},{2,22,3},{3,33,8}]).
increase_age(View) ->
    increase_age(View, []).
increase_age([], New_view) -> New_view;
increase_age([{Id, Pid, Age}|T], New_view) -> increase_age(T, New_view ++ [{Id, Pid, Age+1}]).

select_view(View, C, H, S, Buffer) ->
    Appended_view = View ++ Buffer,
    Removed_duplicate_view = remove_duplicate(Appended_view),
    Removed_old_view = remove_old(Removed_duplicate_view, lists:min([H, (length(Removed_duplicate_view) - C)])),
    Removed_head = remove_head(Removed_old_view, lists:min([S, (length(Removed_old_view) - C)])),
    % ERROR HERE Removed_head is empty
    printList(Removed_head),
    Removed_random = remove_at_random(Removed_head, (length(Removed_head) - C)),
    Removed_random.

remove_duplicate(View) -> remove_duplicate(View, [], []).
remove_duplicate([], Acc, _) -> Acc;
remove_duplicate([{Id, Pid, Age}|T], Acc, SeenIds) -> 
    case lists:member(Id, SeenIds) of true ->
        remove_duplicate(T, Acc, SeenIds);
    false ->
        remove_duplicate(T, [findYoungest(T, {Id, Pid, Age})] ++ Acc, SeenIds ++ [Id])
    end.

findYoungest([], Youngest) -> Youngest;
findYoungest([{Id, Pid, Age}|T], {YId, _, YAge}) when ((Id =:= YId) and (Age < YAge)) ->
    findYoungest(T, {Id, Pid, Age});
findYoungest([{Id, _, Age}|T], {YId, YPid, YAge}) when ((Id =:= YId) and (Age >= YAge)) ->
    findYoungest(T, {YId, YPid, YAge});
findYoungest([{Id, _, _}|T], {YId, YPid, YAge}) when Id =/= YId ->
    findYoungest(T, {YId, YPid, YAge}).

% utils:remove_old([{1,11,5},{2,22,3},{3,33,8}], 2).
remove_old(View, NbToRemove) when NbToRemove < 0 -> View;
remove_old(View, NbToRemove) when NbToRemove >= 0 ->
    Sorted_age = lists:keysort(3, View),
    Elem_to_remove = lists:sublist(Sorted_age, length(Sorted_age)-NbToRemove+1, length(Sorted_age)),
    delete_all_in(View, Elem_to_remove).

remove_head(View, NbToRemove) when NbToRemove < 0 -> View;
remove_head(View, NbToRemove) when NbToRemove >= 0 ->
    lists:sublist(View, length(View)-NbToRemove+1, length(View)).

% utils:remove_at_random([{1,11,5},{2,22,3},{3,33,8}], 1).
remove_at_random(View, NbToRemove) when NbToRemove < 0 -> View;
remove_at_random(View, NbToRemove) when NbToRemove >= 0 ->
    Shuffled_view = permute(View),
    Elem_to_remove = lists:sublist(Shuffled_view, length(Shuffled_view)-NbToRemove+1, length(Shuffled_view)),
    delete_all_in(View, Elem_to_remove).