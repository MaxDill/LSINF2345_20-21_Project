-module(utils).
-export([printList/1, select_view/5, permute/1, move_oldest/2, select_peer/2, head/2, increase_age/1]). 

% Prints a list of strings or atoms
printList([]) -> empty_list;
printList([H|[]]) -> io:format("~p~n",[H]);
printList([H|T]) ->
    io:format("~p, ",[H]),
    printList(T).

% utils:permute([{1,11,8},{2,22,6},{3,33,4}]). 
permute(X) ->
    [Y||{_,Y} <- lists:sort([ {rand:uniform(), N} || N <- X])].

% utils:move_oldest([{1,11,8},{2,22,6},{3,33,4}], 2). 
move_oldest(View, H) ->
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