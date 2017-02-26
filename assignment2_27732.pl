candidate_number(27732).

solve_task(Task,Cost) :-
  ( part(1) -> solve_task_1_3(Task, Cost)
  ; part(3) -> solve_task_1_3(Task, Cost)
  ; part(4) -> solve_task_4(Task, Cost)
  ).

%%%%%%%%%% Part 1 & 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_1_3(Task,Cost) :- % NOTE OLD
  agent_current_position(oscar,P),
  solve_task_bt(Task,[c(0,0,P),P],0,R,Cost,_NewPos),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  agent_do_moves(oscar,Path).

solve_task_1_3_new(Task, Cost) :- % TODO update name
  agent_current_position(oscar, Pos),
  calc_fvalue(Task, Pos, 0, FCost),
  solve_task_astar(Task, [[c(FCost, 0, Pos), Pos]], ReversedPath, Cost, _),!,
  reverse(ReversedPath, [_Init|Path]),
  agent_do_moves(oscar,Path).

%%%%%%%%%% Part 1 & 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Part1: A star search
solve_task_astar(Task, [Current|_], ReversedPath, [cost(Cost),depth(Depth)], NewPos) :-
  achieved(Task, Current, ReversedPath, Cost, NewPos),
  length(ReversedPath, Depth).
solve_task_astar(Task, Agenda, ReversedPath, Cost, NewPos) :-
  Agenda = [Current|AgendaTail], % Get current node
  find_children(Task, Current, Children),
  insert_many_into_agenda(Children, AgendaTail, NewAgenda),
  solve_task_astar(Task, NewAgenda, ReversedPath, Cost, NewPos).

insert_many_into_agenda([], Agenda, Agenda).
insert_many_into_agenda([Child|Children], Agenda, NewAgenda) :-
  insert_into_agenda(Child, Agenda, TempAgenda), % TempAgenda is Agenda without Child
  insert_many_into_agenda(Children, TempAgenda, NewAgenda).

insert_into_agenda(Node,Agenda,Agenda) :- repeat_node(Node,Agenda), ! .
insert_into_agenda(Node,[A|R],[Node,A|R]) :- cheaper(Node,A), ! .
insert_into_agenda(Node,[A|R],[A|S]) :- insert_into_agenda(Node,R,S), !.
insert_into_agenda(Node,[],[Node]).

repeat_node([c(_, _, Pos)|_], [[c(_, _, Pos)|_]|_]).
cheaper([c(FCost1, _, _)|_], [c(FCost2, _, _)|_]) :- FCost1 <  FCost2.

find_children(Task, Node, Children) :-
  Node = [c(_, GCost, NodePos)|Path],
  (bagof([c(ChildFCost, ChildGCost, ChildPos), ChildPos|Path],
    ( search(NodePos, ChildPos, ChildPos, C),
      \+ memberchk(ChildPos, Path), % Don't unclude if already visited
      ChildGCost is GCost + C,
      calc_fvalue(Task, ChildPos, ChildGCost, ChildFCost)
    ), Children); Children = []). % Fill children or empty

calc_fvalue(find(_), _, GCost, FCost) :-
  FCost is GCost.
calc_fvalue(go(TargetPos), Pos, GCost, FCost) :-
  map_distance(Pos, TargetPos, HCost),
  FCost is GCost + HCost.

%%%%%%%%%% Part 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% find_identity(A) :-
%   % Initially all actors are suspects
%   findall(PotentialActor, actor(PotentialActor), PotentialActors),
%   find_identity(A, PotentialActors),!.
%
% find_identity(Actor, [Actor]). % Return when we have one answer
% find_identity(Actor, PotentialActors) :-
%   Charging_Stations is [(1, PosC1), (2, PosC2)],
%   find_charging_station_positions([1, 2]),
%   agent_ask_oracle(oscar, o(1), link, Link),
%   actors_with_link(Link, PotentialActors, [], ReducedPotentialActors),
%   find_identity(Actor, ReducedPotentialActors).
%
%   % Finds list of actors containg Link from Actors list, effectivly map reduce
% actors_with_link(_, [], ActorsWithLink, ActorsWithLink). % Once list exhausted
% actors_with_link(Link, [Actor|Actors], WorkingActorsWithLink, ActorsWithLink) :-
%     wp(Actor, WT),
%     findall(X, wt_link(WT, X), Links),
%     ( memberchk(Link, Links) -> actors_with_link(Link, Actors, [Actor|WorkingActorsWithLink], ActorsWithLink)
%     ; otherwise              -> actors_with_link(Link, Actors, WorkingActorsWithLink, ActorsWithLink)
%     ).
%
% % Return the pos found by a star
move_to_task(Task, Cost, FoundID, FoundType) :- % TODO update name
  agent_current_position(oscar, Pos),
  calc_fvalue(Task, Pos, 0, FCost),
  solve_task_astar(Task, [[c(FCost, 0, Pos), Pos]], ReversedPath, Cost, _),!,
  reverse(ReversedPath, [_Init|Path]),
  agent_do_partial_moves(Path, FoundID, FoundType).

% Find the positions of the charging stations
find_charging_station_positions([], Charging_Stations, Charging_Stations).
find_charging_station_positions(Unvisited_Charging_Stations, Working_Charging_Stations, Charging_Stations) :-
    Unvisited_Charging_Stations = [Next_Charging_Station|CSs],
    move_to_task(find(c(Next_Charging_Station)), _, FoundID, FoundType),
    agent_current_position(oscar, Pos),
    ( char_code("c", C), FoundType = C, map_adjacent(Pos, Adj, c(FoundID)) ->
        ( memberchk(FoundID, CSs)       -> delete(CSs, FoundID, NewCSs), find_charging_station_positions([Next_Charging_Station|NewCSs], [Adj|Working_Charging_Stations], Charging_Stations)
        ; FoundID = Next_Charging_Station  -> find_charging_station_positions(CSs, [Adj|Working_Charging_Stations], Charging_Stations)
        ; otherwise -> find_charging_station_positions(Unvisited_Charging_Stations, Working_Charging_Stations, Charging_Stations)
        )
    ; char_code("o", C), FoundType = C -> find_charging_station_positions(Unvisited_Charging_Stations, Working_Charging_Stations, Charging_Stations)
    ).



agent_do_partial_moves([], _, _).
agent_do_partial_moves([NextPos|Path], FoundID, FoundType) :-
    agent_do_moves(oscar, [NextPos]),
    findall(F, map_adjacent(NextPos, _, F), Fs),
    ( memberchk(c(ID), Fs) -> FoundID is ID, FoundType is "c"
    ; memberchk(o(ID), Fs) -> FoundID is ID, FoundType is "o"
    ; otherwise            -> agent_do_partial_moves(Path, FoundID, FoundType)
    ).


%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_4(Task,Cost) :-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).
%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :-
  achieved(Task,Current,RPath,Cost,NewPos).
solve_task_bt(Task,Current,D,RR,Cost,NewPos) :-
  Current = [c(_,F,P)|RPath],
  search(P,P1,R,C),
  \+ memberchk(R,RPath),  % check we have not been here already
  D1 is D+1,
  F1 is F+C,
  solve_task_bt(Task,[c(_,F1,P1),R|RPath],D1,RR,Cost,NewPos).  % backtrack search

achieved(go(Exit),Current,RPath,Cost,NewPos) :-
  Current = [c(_,Cost,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(_,Cost,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).
