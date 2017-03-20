

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
  query_world(agent_do_moves,[oscar,Path]).

solve_task_1_3_new(Task, Cost) :- % TODO update name
  agent_current_position(oscar, Pos),
  calc_fvalue(Task, Pos, 0, FCost),
  solve_task_astar(Task, [[c(FCost, 0, Pos), Pos]], ReversedPath, Cost, _),!,
  reverse(ReversedPath, [_Init|Path]),
  query_world(agent_do_moves,[oscar,Path]).

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

%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solve_task_4(Task,Cost) :-
%   my_agent(Agent),
%   query_world( agent_current_position, [Agent,P] ),
%   solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos),!,  % prune choice point for efficiency
%   reverse(R,[_Init|Path]),
%   query_world( agent_do_moves, [Agent,Path] ).
%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% NOTE: Only checks for obsticale or another agent in the way
agent_path_free(_, []).
agent_path_free(CurPos, [NextPos|_]) :-
    map_adjacent(CurPos, NextPos, Type),
    (Type == empty; Type == c(_); Type == o(_)).

obsticle.

agent_do_partial_moves([], FoundID, FoundType) :-
  writeln("path is empty"),
  my_agent(Agent),!,
  query_world( agent_current_position, [Agent, CurPos] ),
  findall(F, map_adjacent(CurPos, _, F), Fs),
  ( memberchk(c(ID), Fs) -> FoundID is ID, FoundType = c
  ; memberchk(o(ID), Fs) -> FoundID is ID, FoundType = o
  ; otherwise -> writeln("Somethings gone very wrong")
  ).
agent_do_partial_moves(Path, FoundID, FoundType) :-
  Path = [NextPos|Rest],
  my_agent(Agent),!,
  query_world( agent_current_position, [Agent, CurPos] ),
  writeln("inside agent_do_partial_moves: after query world"),
  (\+ agent_path_free(CurPos, Path) ->
    FoundID is 0, FoundType = obsticle, writeln("anget_path_free returned: obsticle found")
  ; otherwise ->
    writeln("No obstacle"),
    writeln("Next positin" + NextPos),
    query_world(agent_do_moves,[Agent,[NextPos]]),
    writeln("after do moves"),
    findall(F, map_adjacent(NextPos, _, F), Fs),
    writeln("after findall"),
    ( memberchk(c(ID), Fs) -> FoundID is ID, FoundType = c
    ; memberchk(o(ID), Fs) -> FoundID is ID, FoundType = o
    ; otherwise            -> writeln("empty"),agent_do_partial_moves(Rest, FoundID, FoundType)
    ),!
  ).


% Given a task do the move and return the foundID and FoundType
move_to_task(Task, Cost, FoundID, FoundType) :- % TODO update name
  my_agent(Agent),!,
  query_world(agent_current_position,[Agent, Pos]),
  calc_fvalue(Task, Pos, 0, FCost),
  solve_task_astar(Task, [[c(FCost, 0, Pos), Pos]], ReversedPath, Cost, _),!,
  reverse(ReversedPath, [_Init|Path]),
  writeln("inside move_to_task: running agent_do_partial_moves"),
  writeln("Path: " + Path),
  agent_do_partial_moves(Path, FoundID, FoundType),
  writeln("FoundID"+ FoundID),
  writeln("FoundType"+ FoundType),!.

% Find the positions of the charging stations
find_charging_station_positions([], Charging_Stations, Charging_Stations, UpdatedUO, UpdatedUO).
find_charging_station_positions(Unvisited_Charging_Stations, Working_Charging_Stations, Charging_Stations, UO, UpdatedUO) :-
    Unvisited_Charging_Stations = [Next_Charging_Station|CSs],

    my_agent(Agent),!,
    % move_to_task(Task, Cost, FoundID, FoundType)
    writeln("about to move_to_task"),
    move_to_task(find(c(Next_Charging_Station)), _, FoundID, FoundType),
    writeln("inside find_charging_station_positions"),
    query_world(agent_current_position,[Agent, Pos]),
    ( FoundType = c ->
        ( memberchk(FoundID, CSs)       -> delete(CSs, FoundID, NewCSs), find_charging_station_positions([Next_Charging_Station|NewCSs], [Pos|Working_Charging_Stations], Charging_Stations, UO, UpdatedUO)
        ; FoundID = Next_Charging_Station  -> find_charging_station_positions(CSs, [Pos|Working_Charging_Stations], Charging_Stations, UO, UpdatedUO)
        ; otherwise -> find_charging_station_positions(Unvisited_Charging_Stations, Working_Charging_Stations, Charging_Stations, UO, UpdatedUO)
        )
    ; FoundType = o ->
      ( memberchk((FoundID, _), UO) ->
          writeln("Found an unseen oracle"),
          writeln(UO),
          delete(UO, (FoundID, _), WorkingUpdatedUO),
          writeln("deleted"),
          append([(FoundID,Pos)], WorkingUpdatedUO, WorkingUO),
          writeln("appened")
        ; otherwise ->
            writeln("Have already seen this oracle"),
            WorkingUO = UO
        ),
        find_charging_station_positions(Unvisited_Charging_Stations, Working_Charging_Stations, Charging_Stations, WorkingUO, UpdatedUO)
    ; FoundType = obsticle ->
      writeln("inside find_charging_station_positions: FoundType = obsticle"),
      find_charging_station_positions(Unvisited_Charging_Stations, Working_Charging_Stations, Charging_Stations, UO, UpdatedUO)
    ).


% TODO Clean up
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
    my_agent(Agent),!,
    findall(
        Pos,
        (member((_, Pos), UO), Pos = p(_,_)),
        Poss
    ),
    query_world(agent_current_position,[Agent, CurPos]),
    closest_position(CurPos, Poss, ClosestPos),
    Task = go(ClosestPos).

agent_pick_task(Task, Task, _, 0). % Use the same task if no reevalutation
agent_pick_task(go(Pos), NewTask, Charging_Stations, 1) :- % If going to an oracle at known position
    my_agent(Agent),!,
    query_world(agent_current_energy,[Agent, E]),
    query_world(agent_current_position,[Agent, CurPos]),
    map_distance(CurPos, Pos, EstimatedCostToOracle),
    closest_position(Pos, Charging_Stations, OracleChargingStationPos),
    map_distance(Pos, OracleChargingStationPos, EstimatedCostFromOracleToCharging),
    Cost is (EstimatedCostToOracle + EstimatedCostFromOracleToCharging + 10),

    ( Cost < (E - 15) -> NewTask = go(Pos)
    ; otherwise -> closest_position(CurPos, Charging_Stations, ChargingStationPos), NewTask = go(ChargingStationPos)
    ).
agent_pick_task(find(T), NewTask, Charging_Stations, 1) :- % If going to an oracle at an unknown position
    my_agent(Agent),!,
    query_world(agent_current_energy,[Agent, E]),
    query_world(agent_current_position,[Agent, CurPos]),
    ( E < 100   -> closest_position(CurPos, Charging_Stations, ChargingStationPos), NewTask = go(ChargingStationPos)
    ; otherwise -> NewTask = find(T)
    ).

do_action(_, _, go(exit), _, _, _, _, _, 1) :- false.
do_action(_, UO, _, 0, obsticle, UO, PotentialActors, PotentialActors, 1) :-
  writeln("obsticle found").
do_action(_, UO, _, ObjectID, c, UO, PotentialActors, PotentialActors, 1) :- my_agent(Agent),!,writeln('update energy'), query_world(agent_topup_energy,[Agent, c(ObjectID)]).
do_action(Charging_Stations, UO, Task, ObjectID, o, UpdatedUO, PotentialActors, ReducedPotentialActors, Reevaluate) :- % Heading to an oracle, see another oracle so query
   my_agent(Agent),!,
   writeln("task is find(o) or go(Pos)"),
   ( Task = go(Pos), memberchk(Pos, Charging_Stations) ->
       Reevaluate = 1,
       ReducedPotentialActors = PotentialActors,
       writeln("Pos is a charging_station"),
       ( memberchk((ObjectID, _), UO) ->
           writeln("oracle in unqueried list"),
           % potentially change
           delete(UO, (ObjectID, _), WorkingUpdatedUO),
           query_world(agent_current_position,[Agent, CurPos]),
           UpdatedUO = [(ObjectID,CurPos)|WorkingUpdatedUO]
       ; otherwise ->
           writeln("have already queried oracle, returning true"),
           UpdatedUO = UO
       )
   ; otherwise ->
       ( Task = go(Pos), \+ memberchk(Pos, Charging_Stations)
       ; Task = find(o(_))
       ),
       writeln("Object in unvisted"),
       ( memberchk((ObjectID, _), UO) ->
          writeln('asking oracle'),
          writeln("agent: " + Agent + "ObjectID: " + ObjectID),
          query_world(agent_ask_oracle,[Agent, o(ObjectID), link, Link]),
          writeln("before delete"),
          delete(UO, (ObjectID, _), UpdatedUO),
          writeln("after delete"),
          actors_with_link(Link, PotentialActors, [], ReducedPotentialActors),
          writeln("going wrong!!"),
          Reevaluate = 1
        ; otherwise ->
          writeln("have already queried oracle, returning true"),
          UpdatedUO = UO,
          ReducedPotentialActors = PotentialActors,
          Reevaluate = 0 % Don't re-evaluate, this stops a loop where searching for an oracles, comes across a discovered oracle, and returning to a charging station only to come across the discocered oracle again
        )
    ), writeln("end of do_action").
% do_action(Charging_Stations, UO, go(Pos), ObjectID, o, UpdatedUO, PotentialActors, PotentialActors, 1) :-
%     my_agent(Agent),!,
%     writeln("Inside do_action: oracle found, checking if agent is moving to charging station"), %TODO code breaks here, returns false
%     writeln("Curpos" + Pos),
%     memberchk(Pos, Charging_Stations),% Heading to a charging station, ignore
%     writeln("Pos is a charging_station"),
%     ( memberchk((ObjectID, _), UO) ->
%         writeln("oracle in unqueried list"),
%         % potentially change
%         delete(UO, (ObjectID, _), WorkingUpdatedUO),
%         query_world(agent_current_position,[Agent, CurPos]),
%         UpdatedUO = [(ObjectID,CurPos)|WorkingUpdatedUO]
%     ; otherwise ->
%         writeln("have already queried oracle, returning true"),
%         UpdatedUO = UO
%     ).



solve_task_3(Actor) :-
    % TODO update positions of oracles when looking for charging stations and in do action
    UO = [(1, false), (2, false), (3, false), (4, false), (5, false), (6, false), (7, false), (8, false), (9, false), (10, false)],
    find_charging_station_positions([1,2], [], CS, UO, UpdatedUO),
    writeln("Starting solve task executing:  " ),
    findall(PotentialActor, actor(PotentialActor), PotentialActors),
    solve_task_3(Actor, PotentialActors, UpdatedUO, CS, 1),!.

solve_task_3(Actor, [Actor], _, _, _).
solve_task_3(Actor, PotentialActors, UO, CSs, Reevaluate) :-
    writeln("Starting solve task executing:  " ),
    writeln("PotentialActors:  " + PotentialActors ),

    writeln("Finding next Oracle executing: " + UO),
    find_next_oracle(UO, ProposedTask),!,

    writeln("anget_pick_task:  " ),
    agent_pick_task(ProposedTask, Task, CSs, Reevaluate),
    writeln("Proposed Task: " + ProposedTask ),
    writeln("New Task: " + Task ),

    writeln("move_to_task:  " ),
    % TODO:: May need to change move_to_task
    move_to_task(Task, _, OID, OType),
    writeln("OID:  " + OID ),
    writeln("OType:  " + OType ),

    writeln("do_action executing:  " ),
    do_action(CSs, UO, Task, OID, OType, UpdatedUO, PotentialActors, UpdatedPotentialActors, NewReevaluate),
    writeln("updated potential actors" + UpdatedPotentialActors),
    writeln("------------------------ NEXT ITERATION --------------------------"),
    flush_output,
    solve_task_3(Actor, UpdatedPotentialActors, UpdatedUO, CSs, NewReevaluate).


%%%%
solve_task_4(_Task, _Cost) :-
  join_game(_Agent),
  game_predicates:ailp_reset,
  start_game,
  solve_task_3(Actor),
  writeln("Actor Found " + Actor).


  reset_debug(NewPos) :-
      my_agent(Agent),
      agent_current_position(Agent,Pos),
      retract(ailp_internal(agent_position(Agent, Pos))),
      assert(ailp_internal(agent_position(Agent, NewPos))).

%%%%

% solve_task_3() :-
%   agent_current_position(oscar, Pos),
%   find_charging_station_positions(Pos, [1,2], Charging_Stations),
%
%   Unvisted_Oracles is [1,2,3,4,5,6,7,8,9,10],
%   Unvisted_Oracles_With_Known_Pos is [],
%   find_next_oracle(Pos, Unvisted_Oracles_With_Known_Pos, Unvisted_Oracles, Task)
%   agent_pick_task(Task, NewTask, ?),



  % % No oracles or charging stations found on the way
  % move_to_task(Task, Cost, FoundID, FoundType),
  %
  % % query the oracle
  % do_action(Task, FoundID, FoundType). %


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
