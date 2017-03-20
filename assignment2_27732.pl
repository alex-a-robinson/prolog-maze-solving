

candidate_number(27732).

solve_task(Task,Cost) :-
  ( part(1) -> solve_task_1_3(Task, Cost)
  ; part(3) -> solve_task_1_3(Task, Cost)
  ; part(4) -> solve_task_4(Task, Cost)
  ).

%%%%%%%%%% Part 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_1_3(Task, Cost) :-
  agent_current_position(oscar, Pos),
  calc_fvalue(Task, Pos, 0, FCost),
  solve_task_astar(Task, [[c(FCost, 0, Pos), Pos]], ReversedPath, Cost, _),!, % prune choice point for efficiency
  reverse(ReversedPath, [_Init|Path]),
  agent_do_moves(oscar,Path).

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
obsticle.

% NOTE: Only checks for obsticale or another agent in the way
agent_path_free(_, []).
agent_path_free(CurPos, [NextPos|_]) :-
    map_adjacent(CurPos, NextPos, Type),
    (Type == empty; Type == c(_); Type == o(_)).

agent_do_partial_moves([], FoundID, FoundType) :-
  (part(4) ->
    my_agent(Agent),!,
    query_world( agent_current_position, [Agent, CurPos] )
  ;otherwise ->
    agent_current_position(oscar, CurPos)
  ),
  findall(F, map_adjacent(CurPos, _, F), Fs),
  ( memberchk(c(ID), Fs) -> FoundID is ID, FoundType = c
  ; memberchk(o(ID), Fs) -> FoundID is ID, FoundType = o
  ).
agent_do_partial_moves(Path, FoundID, FoundType) :-
  Path = [NextPos|Rest],
  (part(4) ->
    my_agent(Agent),!,
    query_world( agent_current_position, [Agent, CurPos] )
  ;otherwise ->
    agent_current_position(oscar, CurPos)
  ),
  (\+ agent_path_free(CurPos, Path) ->
    FoundID is 0, FoundType = obsticle
  ; otherwise ->
      (part(4) ->
        my_agent(Agent),!,
        query_world( agent_do_moves, [Agent, [NextPos]] )
      ;otherwise ->
        agent_do_moves(oscar, [NextPos])
      ),
    findall(F, map_adjacent(NextPos, _, F), Fs),
    ( memberchk(c(ID), Fs) -> FoundID is ID, FoundType = c
    ; memberchk(o(ID), Fs) -> FoundID is ID, FoundType = o
    ; otherwise            -> agent_do_partial_moves(Rest, FoundID, FoundType)
    ),!
  ).

% Given a task do the move and return the foundID and FoundType
move_to_task(Task, Cost, FoundID, FoundType) :- % TODO update name
  (part(4) ->
    my_agent(Agent),!,
    query_world( agent_current_position, [Agent, Pos] )
  ;otherwise ->
    agent_current_position(oscar, Pos)
  ),
  calc_fvalue(Task, Pos, 0, FCost),
  solve_task_astar(Task, [[c(FCost, 0, Pos), Pos]], ReversedPath, Cost, _),!,
  reverse(ReversedPath, [_Init|Path]),
  agent_do_partial_moves(Path, FoundID, FoundType).

% Find the positions of the charging stations
find_charging_station_positions([], Charging_Stations, Charging_Stations, UpdatedUO, UpdatedUO).
find_charging_station_positions(Unvisited_Charging_Stations, Working_Charging_Stations, Charging_Stations, UO, UpdatedUO) :-
    Unvisited_Charging_Stations = [Next_Charging_Station|CSs],
    move_to_task(find(c(Next_Charging_Station)), _, FoundID, FoundType),
    (part(4) ->
      my_agent(Agent),!,
      query_world( agent_current_position, [Agent, Pos] )
    ;otherwise ->
      agent_current_position(oscar, Pos)
    ),
    ( FoundType = c ->
        ( memberchk(FoundID, CSs)       -> delete(CSs, FoundID, NewCSs), find_charging_station_positions([Next_Charging_Station|NewCSs], [Pos|Working_Charging_Stations], Charging_Stations, UO, UpdatedUO)
        ; FoundID = Next_Charging_Station  -> find_charging_station_positions(CSs, [Pos|Working_Charging_Stations], Charging_Stations, UO, UpdatedUO)
        ; otherwise -> find_charging_station_positions(Unvisited_Charging_Stations, Working_Charging_Stations, Charging_Stations, UO, UpdatedUO)
        )
    ; FoundType = o ->
      ( memberchk((FoundID, _), UO) ->
          delete(UO, (FoundID, _), WorkingUpdatedUO),
          append([(FoundID,Pos)], WorkingUpdatedUO, WorkingUO)
        ; otherwise ->
            WorkingUO = UO
        ),
        find_charging_station_positions(Unvisited_Charging_Stations, Working_Charging_Stations, Charging_Stations, WorkingUO, UpdatedUO)
    ; FoundType = obsticle ->
      find_charging_station_positions(Unvisited_Charging_Stations, Working_Charging_Stations, Charging_Stations, UO, UpdatedUO)
    ).

closest_position(Pos, CSs, ClosestPos) :- closest_position(Pos, CSs, 9999, p(0,0), ClosestPos), !.
closest_position(_, [], _, Out, Out).
closest_position(Pos, [CS|CSs], CurrentClosestCost, ClosestPos, Out) :-
  map_distance(Pos, CS, Cost),
  ( Cost < CurrentClosestCost -> closest_position(Pos, CSs, Cost, CS, Out)
  ; otherwise -> closest_position(Pos, CSs, CurrentClosestCost, ClosestPos, Out)
  ).

find_next_oracle([], go(exit)). % Exit if no oracles left
find_next_oracle(UO, Task) :- % If no oracles with known positions, pick first unvisited ID
    \+ memberchk((_, p(_,_)), UO), % if There are no oracles with known positions
    UO = [(ID, _)|_],
    Task = find(o(ID)).
find_next_oracle(UO, Task) :- % If no oracles with known positions, pick first unvisited ID
    findall(
        Pos,
        (member((_, Pos), UO), Pos = p(_,_)),
        Poss
    ),
    (part(4) ->
      my_agent(Agent),!,
      query_world( agent_current_position, [Agent, CurPos] )
    ;otherwise ->
      agent_current_position(oscar, CurPos)
    ),
    closest_position(CurPos, Poss, ClosestPos),
    Task = go(ClosestPos).

agent_pick_task(Task, Task, _, 0). % Use the same task if no reevalutation
agent_pick_task(go(Pos), NewTask, Charging_Stations, 1) :- % If going to an oracle at known position
    (part(4) ->
      my_agent(Agent),!,
      query_world( agent_current_position, [Agent, CurPos] ),
      query_world(agent_current_energy,[Agent, E])
    ;otherwise ->
      agent_current_position(oscar, CurPos),
      agent_current_energy(oscar, E)
    ),
    map_distance(CurPos, Pos, EstimatedCostToOracle),
    closest_position(Pos, Charging_Stations, OracleChargingStationPos),
    map_distance(Pos, OracleChargingStationPos, EstimatedCostFromOracleToCharging),
    Cost is (EstimatedCostToOracle + EstimatedCostFromOracleToCharging + 10),

    ( Cost < (E - 15) -> NewTask = go(Pos)
    ; otherwise -> closest_position(CurPos, Charging_Stations, ChargingStationPos), NewTask = go(ChargingStationPos)
    ).
agent_pick_task(find(T), NewTask, Charging_Stations, 1) :- % If going to an oracle at an unknown position
    (part(4) ->
      my_agent(Agent),!,
      query_world( agent_current_position, [Agent, CurPos] ),
      query_world(agent_current_energy,[Agent, E])
    ;otherwise ->
      agent_current_position(oscar, CurPos),
      agent_current_energy(oscar, E)
    ),
    ( E < 100   -> closest_position(CurPos, Charging_Stations, ChargingStationPos), NewTask = go(ChargingStationPos)
    ; otherwise -> NewTask = find(T)
    ).

do_action(_, _, go(exit), _, _, _, _, _, 1) :- false.
do_action(_, UO, _, 0, obsticle, UO, PotentialActors, PotentialActors, 1).
do_action(_, UO, _, ObjectID, c, UO, PotentialActors, PotentialActors, 1) :-
  (part(4) ->
    my_agent(Agent),!,
    query_world( agent_topup_energy, [Agent, c(ObjectID)] )
  ;otherwise ->
    agent_topup_energy(oscar, c(ObjectID))
  ).
do_action(Charging_Stations, UO, Task, ObjectID, o, UpdatedUO, PotentialActors, ReducedPotentialActors, Reevaluate) :- % Heading to an oracle, see another oracle so query
   ( Task = go(Pos), memberchk(Pos, Charging_Stations) ->
       Reevaluate = 1,
       ReducedPotentialActors = PotentialActors,
       ( memberchk((ObjectID, _), UO) ->
           delete(UO, (ObjectID, _), WorkingUpdatedUO),
           (part(4) ->
             my_agent(Agent),!,
             query_world( agent_current_position, [Agent, CurPos] )
           ;otherwise ->
             agent_current_position(oscar, CurPos)
           ),
           UpdatedUO = [(ObjectID,CurPos)|WorkingUpdatedUO]
       ; otherwise ->
           UpdatedUO = UO
       )
   ; otherwise ->
       ( Task = go(Pos), \+ memberchk(Pos, Charging_Stations)
       ; Task = find(o(_))
       ),
       ( memberchk((ObjectID, _), UO) ->
          (part(4) ->
            my_agent(Agent),!,
            query_world(agent_ask_oracle,[Agent, o(ObjectID), link, Link])
          ;otherwise ->
            agent_ask_oracle(oscar, o(ObjectID), link, Link)
          ),
          delete(UO, (ObjectID, _), UpdatedUO),
          actors_with_link(Link, PotentialActors, [], ReducedPotentialActors),
          Reevaluate = 1
        ; otherwise ->
          UpdatedUO = UO,
          ReducedPotentialActors = PotentialActors,
          Reevaluate = 0 % Don't re-evaluate, this stops a loop where searching for an oracles, comes across a discovered oracle, and returning to a charging station only to come across the discocered oracle again
        )
    ).

find_identity(Actor) :-
    UO = [(1, false), (2, false), (3, false), (4, false), (5, false), (6, false), (7, false), (8, false), (9, false), (10, false)],
    find_charging_station_positions([1,2], [], CS, UO, UpdatedUO),
    findall(PotentialActor, actor(PotentialActor), PotentialActors),
    find_identity(Actor, PotentialActors, UpdatedUO, CS, 1),!.

find_identity(Actor, [Actor], _, _, _).
find_identity(Actor, PotentialActors, UO, CSs, Reevaluate) :-
    find_next_oracle(UO, ProposedTask),!,
    agent_pick_task(ProposedTask, Task, CSs, Reevaluate),
    move_to_task(Task, _, OID, OType),
    do_action(CSs, UO, Task, OID, OType, UpdatedUO, PotentialActors, UpdatedPotentialActors, NewReevaluate),
    find_identity(Actor, UpdatedPotentialActors, UpdatedUO, CSs, NewReevaluate).

%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_4(Actor) :-
  join_game(_Agent),
  game_predicates:ailp_reset,
  start_game,
  find_identity(Actor),
  writeln("Actor Found " + Actor).

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
