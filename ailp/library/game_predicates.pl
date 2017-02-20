/*
 * game_predicates.pl
 *
 * Do not call the exported predicates directly, but access them via http through oscar_library.pl!
 *
 * Dynamic behaviour of the world is also controlled here in a separate thread,
 * see the 'changeable parameters' section near the top of the file.
 */

:- module(game_predicates,
  [ agent_current_position/2,
    agent_current_energy/2,
    agent_topup_energy/2,
    agent_ask_oracle/4,
    agent_check_oracle/2,
    agent_do_moves/2,
    internal_leave_game/1,
    internal_join_game/1,
    check_pos/2,
    game_status/1,
    internal_start_game/0,
    ailp_reset/0
  ]
).

:- use_module('../command_channel.pl').

:- dynamic
  ailp_internal/1,
  dynamic_thread/1.

%%% These parameters can be changed %%%
max_players(10).
internal_grid_size(20).  % may be changed in testing

%%% Control dynamic movement of Things, Charging Stations, Oracles here:
% dynamic_params( Th, Ch, Or, T ) <-
% every T seconds a random selection of objects moves one step in a random direction
% the first three parameters are of the form [N,Dirs]
% where N indicates the number of objects that move
% and Dirs subset [n,e,s,w] gives the possible directions

%dynamic_params( [0,[]], [0,[]], [0,[]], 100 ).  % no movement
dynamic_params([10,[n,e,s,w]], [0,[]], [0,[]], 20).  % 10 things move every 20 seconds in all directions
%dynamic_params( [40,[n,s]], [2,[w,e]], [2,[n,s,w,e]], 20 ).  % a lot more movement

%%% End of changeable parameters %%%


% assign drawable features to the agent
% DrawableA has format [AgentId, Shape(n sides), Colour, initialX, initialY]
drawable_agent(A , DrawableA) :-
  random(3, 10, S),
  random_member(C, [purple, blue, beige, white, gray, pink]),!,
  random_member(Cpath, [purple, blue, beige, white, gray, pink]),!,
  assert(ailp_internal(agent_colour_path(A, Cpath))),
  random_free_pos(p(X,Y)),
  assert(ailp_internal(agent_position(A, p(X,Y)))),
  internal_topup(Emax),
  assert(ailp_internal(agent_energy(A, Emax))),
  DrawableA = [A,S,C,X,Y].

% assigns a unique numerical id for a new agent
internal_join_game(Agent) :-
  \+game_status(running),
  max_players(Max),
  new_agent_id(Max, Agent),!,
  assert(ailp_internal(agent_energy(Agent, 0))), %temp assert so that ids can be retrieved by ailp_reset
  retractall(ailp_internal(game_status(ready))),
  atomic_list_concat(['Agent ',Agent,' joined game'], Message),
  do_command([Agent,console,Message]).

%randomly generate an unique id for a new agent
new_agent_id(Max,Id) :-
  random(1, Max, PossId),
  ( agent_current_energy(PossId,_) -> new_agent_id(Max,Id)
  ; otherwise                      -> Id = PossId
  ).

game_status(S) :-
  ailp_internal(game_status(S)).

% change game status
internal_start_game :-
  % check if map is draw/updated
  ( \+game_status(ready) -> atomic_list_concat(['Reset the map before starting the game.'], Message),
                            do_command([dyna,console,Message]), fail
  ; otherwise            -> retract(ailp_internal(game_status(_))),
                            assert(ailp_internal(game_status(running)))
  ),
  ( dynamic_thread(_) -> true
  ; otherwise -> % continuously move objects on the grid in a separate thread
                 dynamic_params(Th,Ch,Or,Period),
                 thread_create( dynamic_grid(Th,Ch,Or,Period), ThreadID, []),
                 assert(dynamic_thread(ThreadID))
  ).

random_free_pos(P) :-
  internal_grid_size(N),
  random(1,N,X),
  random(1,N,Y),
  ( check_pos(p(X,Y), empty) -> (P = p(X,Y),!)
  ; otherwise                -> random_free_pos(P)
  ).

%---------------------------- agent predicates --------------------------------%

% agent_current_energy(+Agent, -Energy)
agent_current_energy(Agent, Energy) :-
  nonvar(Agent),
  var(Energy),
  ailp_internal(agent_energy(Agent,Energy)),
  atomic_list_concat(['Current energy:',Energy],' ',A),
  do_command([Agent,console,A]).

% agent_current_position(+Agent, -Pos)
agent_current_position(Agent, Pos) :-
  nonvar(Agent),
  var(Pos),
  ailp_internal(agent_position(Agent,Pos)).

% agent_topup_energy(+Agent, +OID)
% Agent's position needs to be map_adjacent to charging station identified by OID
agent_topup_energy(Agent, OID) :-
  nonvar(Agent),
  nonvar(OID),
  agent_current_position(Agent,Pos),
  map_adjacent(Pos, _AdjPos, OID),
  OID = c(_),
  retract(ailp_internal(agent_energy(Agent, _E))),
  internal_topup(Emax),
  assert(ailp_internal(agent_energy(Agent,Emax))).

% agent_ask_oracle(+Agent, +OID, +Question, -Answer)
% Agent's position needs to be map_adjacent to oracle identified by OID
% fails if oracle already visited by Agent
agent_ask_oracle(Agent, OID, Question, Answer) :-
  nonvar(Agent),
  nonvar(OID),
  ( game_status(running) -> true
  ; otherwise            -> do_command([Agent, console, 'start the game first']), fail
  ),
  \+ ailp_internal(agent_visited_oracle(Agent, OID)),
  nonvar(Question),
  var(Answer),
  internal_topup(Emax),
  Cost is ceiling(Emax/10),
  ailp_internal(agent_energy(Agent,Energy)),
  ( Energy>Cost -> agent_current_position(Agent,Pos),
                   map_adjacent(Pos, AdjPos, OID),
                   OID = o(_),
                   internal_object(OID, AdjPos, Options),
                   member( question(Q)/answer(A),Options),
                   ( Question=Q -> Answer=A ; Answer='I do not know' ),
                   atomic_list_concat( [Question,Answer],': ',AA),
                   internal_use_energy( Agent,Cost),
                   assert( ailp_internal(agent_visited_oracle(Agent, OID)))
  ; otherwise -> Answer='Sorry, not enough energy',AA=Answer
  ),
  do_command([Agent,console,AA]).

% agent_colour_path(+Agent, ?ColourPath)
agent_colour_path(Agent, ColourPath) :-
  nonvar(Agent),
  ailp_internal(agent_colour_path(Agent, ColourPath)).

% agent_check_oracle(+Agent, +OID)
% checks whether oracle already visited by Agent
agent_check_oracle(Agent, OID) :-
  nonvar(Agent),
  nonvar(OID),
  ailp_internal(agent_visited_oracle(Agent, OID)).

% agent_do_moves(+Agent, +ListOfMoves)
agent_do_moves(_, []).
agent_do_moves(Agent, [H|T]) :-
  agent_do_move(Agent, H),
  agent_do_moves(Agent,T).
% agent_do_move(+Agent, +To)
% Has constraint that To is map_adjacent to Agent's current position
% Reduces energy by 1 if step is valid
agent_do_move(Agent,To) :-
  nonvar(Agent),
  nonvar(To),
  game_status(running),
  agent_current_energy(Agent, F),
  F>0,
  %% check p(X,Y) if To is map_adjacent to current position and free
  agent_current_position(Agent,Pos),
  map_adjacent(Pos, To, Obj),
  Obj = empty,!,
  %% send move to server
  p(X,Y) = To,
  do_command([Agent, move, X, Y], _R),
  agent_colour_path(Agent, Cpath),
  do_command([Agent, colour, X, Y, Cpath]),
  %% move was successful so decrease agent energy
  internal_use_energy(Agent,1),
  retract(ailp_internal(agent_position(Agent, _))),
        assert(ailp_internal(agent_position(Agent, To))).

internal_leave_game(Agent) :-
  retract(ailp_internal(agent_position(Agent,_))),
  retract(ailp_internal(agent_energy(Agent,_))),
  retract(ailp_internal(agent_colour_path(Agent,_))).

%%% global predicates %%%

%ailp_start_position(p(1,1)).

%reset and draw the grid map
ailp_reset :-
  internal_grid_size(N),
  findall(A, (ailp_internal(agent_energy(A,_))), Agents),
  retractall(ailp_internal(_)),
  retractall(my_agent(_)),
  init_things(oracle,N/2),
  init_things(charging_station,N/10),
  init_things(thing,N*N/4),
  wp:init_identity,  % defined in wp.pl
  maplist( drawable_agent, Agents, DrawableAgents),
  append( DrawableAgents, [[dyna, 0,red, 1,1]], AllAgents), % adds a dummy agent to use in do_command predicate
  reset([
    grid_size=N,
    cells=[
      [green, 1,1, N,N]
    ],
    agents = AllAgents
  ]),
  maplist( colour_agent_position, DrawableAgents),
  internal_colour_map,  % make objects visible at the start
  assert(ailp_internal(game_status(ready))).

colour_agent_position([A,_,_,X,Y]) :-
  agent_colour_path( A,Cpath),
  do_command([A, colour, X,Y,Cpath]),
  atomic_list_concat(['starts at p(',X,',',Y,')'], Message),
  do_command([A,console,Message]).

%%% Do not query any of the predicates below! %%%

internal_topup(Emax) :-
  internal_grid_size(N),
  Emax is ceiling(N*N/4).

compute_step(p(X,Y), M, p(X1,Y1), I) :-
  ( M = s -> X1 =  X,    Y1 is Y+I
  ; M = e -> X1 is X+I,  Y1 =  Y
  ; M = n -> X1 =  X,    Y1 is Y-I
  ; M = w -> X1 is X-I,  Y1 =  Y
  ).

internal_poss_step(P0, M, PossPosition, I) :-
  member(M, [s,e,n,w]), % try moves in this order
  compute_step( P0, M, PossPosition, I).

internal_poss_step(P0, M, PossMoves, PossPosition, I) :-
  random_member(M, PossMoves), % moves randomly to any possible direction
  compute_step(P0, M, PossPosition, I).

% check_pos(+Pos, ?OID)
check_pos(Pos, OID) :-
  nonvar(Pos),
  ( internal_off_board(Pos)                               -> fail
  ; bagof( A, ailp_internal(agent_position(A, Pos)), _As) -> fail
  ; internal_object1(O,Pos,_)                             -> OID = O
  ; otherwise                                             -> OID = empty
  ).

internal_off_board(p(X,Y)) :-
  internal_grid_size(N),
  ( X < 1
  ; X > N
  ; Y < 1
  ; Y > N
  ).

internal_use_energy(Agent,Cost) :-
  nonvar(Agent),
  retract(ailp_internal(agent_energy(Agent, E))),
  E>0, E1 is E - Cost,
  assert(ailp_internal(agent_energy(Agent,E1))),
  ( E1 < 20 -> atomic_list_concat(['WARNING -- Low energy:',E1],' ',A),
               do_command([Agent,console,A])
  ; true
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%% WORLD PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The position and number of these objects changes every time ailp_reset/0 is called
internal_object(c(I),Pos,[]) :-
  ailp_internal(charging_station(I,Pos)).
%% Oracles that have information
internal_object(o(I),Pos,[question(link)/answer(Link)]):-
  ailp_internal(oracle(I,Pos)),
  wp:ailp_identity(A),
  wp:random_link(A,Link).
%% Obstacles (things)
internal_object(t(I),Pos,[]) :-
  ailp_internal(thing(I,Pos)).

% version that makes the object visible on the map
internal_object1(O,Loc,L) :-
  internal_object(O,Loc,L).
  %internal_colour_loc(O,Loc).  % disabled as may cause web client overload

% object_colour(+Label, -Colour)
object_colour(Label, Colour) :-
  ( Label = oracle           -> Colour=red
  ; Label = charging_station -> Colour=orange
  ; Label = thing            -> Colour=black).
internal_colour_loc(O,p(X,Y)) :-
  ( O=t(_) -> Colour=black   % obstacle
  ; O=c(_) -> Colour=orange  % charging station
  ; O=o(_) -> Colour=red     % oracle
  ),
  do_command([dyna,colour,X,Y,Colour]).
internal_colour_map :-
  internal_object(O,Loc,_),
  internal_colour_loc(O,Loc),
  fail.
internal_colour_map.

init_things(Label,Exp) :-
  K is ceiling(Exp),   % round up if Exp evaluates to a fraction
  KK = 99999,
  randset(K,KK,S),
  internal_grid_size(N),
  internal_things(S,N,Label,1).

internal_things([],_N,_L,_M).
internal_things([Z|Zs],N,Label,M) :-
  internal_number2pos(Z,N,Pos),
  Fact =.. [Label,M,Pos],
  ( check_pos(Pos, empty) -> assert(ailp_internal(Fact))
  ; otherwise             -> true
  ),
  M1 is M+1,
  internal_things(Zs,N,Label,M1).

internal_number2pos(Z,N,p(X,Y)) :-
  K=N*N/5,  % roughly one in five cells is an obstacle
  Z1 is mod(Z,K),
  Z2 is (Z-Z1)/K,
  X is mod(Z1,N) + 1,
  Y is mod(Z2,N) + 1.

% reposition N things of kind label at a certain frequency rate (in secs)
dynamic_grid([N_things, ThingMoves], [N_chargSts, ChargMoves], [N_oracles, OracleMoves], Period) :-
  game_status(running),
  sleep(Period),  % wait for time to move objects
  findall([I,Pos], ailp_internal( thing(I,Pos)), Things),
  findall([I,Pos], ailp_internal( oracle(I,Pos)), Oracles),
  findall([I,Pos], ailp_internal( charging_station(I,Pos)), ChargSts),
  move_objects(N_chargSts, ChargSts, charging_station, orange, ChargMoves),
  move_objects(N_things, Things, thing, black, ThingMoves),
  move_objects(N_oracles, Oracles, oracle, red, OracleMoves),
  dynamic_grid([N_things, ThingMoves], [N_chargSts, ChargMoves], [N_oracles, OracleMoves], Period).

% move N random objects of kind Label to a random possible direction
% Label can assume thing, charging_station or oracle values
move_objects(0, _, _, _, _).
move_objects(_,[], _, _, _).
move_objects(_, _, _, _,[]).
move_objects(N, Os, Label, Colour, PossMoves) :-
  random_member(O, Os),
  delete(Os,O,L),
  ( move_object(O, Label, Colour,PossMoves) -> N1 is N-1
  ; otherwise                               -> N1 = N
  ),
  move_objects( N1, L, Label, Colour, PossMoves).
move_object([I,Pos], Label, Colour, PossMoves) :-
  Fact =.. [Label,I,Pos],
  map_adjacent_random(Pos, NewPos, PossMoves),
  NewFact =.. [Label, I, NewPos],
  assert(ailp_internal(NewFact)),
  retract(ailp_internal(Fact)),
  Pos = p(X0,Y0),
  NewPos = p(X,Y),
  do_command([dyna, colour, X0, Y0, green]),
  do_command([dyna, colour, X, Y, Colour]).

%map_adjacent_random(+P, ?AdjPos, +PassMoves).
% generates a position adjacent to P
map_adjacent_random(P, AdjPos, PossMoves) :-
  nonvar(P),
  random_member(M, PossMoves),
  compute_step(P,M,PossPos,1),
  ( check_pos(PossPos, empty) -> AdjPos = PossPos
  ; otherwise                 -> delete( PossMoves, M, PossMoves1),
                                  map_adjacent_random(P, AdjPos, PossMoves1)
  ).

map_adjacent(Pos, AdjPos, OID) :-
  nonvar(Pos),
  internal_poss_step(Pos, _M, AdjPos, 1),
  check_pos(AdjPos, OID).
