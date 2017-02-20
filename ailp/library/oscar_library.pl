/*
 *  oscar_library.pl
 *  Only use predicates exported in module heading in your code!
 */

:- module(oscar_library,
  [ %%% Part 1 %%%
    %%% map predicates %%%
    map_adjacent/3,           % ?-map_adjacent(p(1,5),A,O).
    map_distance/3,           % ?-map_distance(p(1,5),p(2,3),D).
    %%% agent predicates %%%
    agent_do_move/2,          % ?-agent_do_move(oscar,p(2,1)).      % must be adjacent
    agent_do_moves/2,         % ?-agent_do_moves(oscar,[p(2,1),p(3,1),p(4,1),p(4,2)]).
    agent_current_energy/2,   % ?-agent_current_energy(oscar,E).
    agent_current_position/2, % ?-agent_current_position(oscar,P).
    agent_topup_energy/2,     % ?-agent_topup_energy(oscar, c(1)).  % must be adjacent
    agent_ask_oracle/4,       % ?-agent_ask_oracle(oscar,o(1)).     % must be adjacent
    %%% global predicates %%%
    ailp_reset/0,             % ?-ailp_reset.
    ailp_start_position/1,    % ?-ailp_start_position(Pos).
    %%% Part 3 %%%
    agent_check_oracle/2,     % ?-agent_check_oracle(oscar,o(1)).
    %%% oscar_message %%%
    say/1,
    %%% assignment part %%%
    part/1,
    %%% moved from oscar.pl file %%%
    shell/0,                   % interactive shell for the grid world
    %%% re-exported from command_channel.pl %%%
    start/0,
    stop/0,
    %%% Part 4 %%%
    query_world/2,
    possible_query/2,
    my_agent/1,
    leave_game/0,
    join_game/1,
    start_game/0,
    reset_game/0
    % map_adjacent/3,
    % map_distance/3
  ]
).

% Part 4 %
:- use_module(library(http/http_client), [http_post/4]).
% Part 4 %

:- use_module('../command_channel.pl').
:- set_homepage('oscar.html').

:- dynamic
   ailp_internal/1,
   ailp_internal_thing/2,
   part/1.

% Define part of the assignment
part(1).

%%%%%%%%%% map predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1 & 3 + 4
% map_adjacent(+Pos, ?AdjPos, ?Occ)
% Occ = empty / c(42) / o(37) - charging station / oracle and ids
map_adjacent(Pos, AdjPos, OID) :-
  nonvar(Pos),
  internal_poss_step(Pos, _M, AdjPos, 1),
  ( part(4)   -> query_world( check_pos, [AdjPos, OID])
  ; otherwise -> ( internal_off_board(AdjPos) -> fail
                 ; internal_object1(O,AdjPos,_) -> OID = O
                 ; otherwise -> OID = empty
                 )
  ).

% map_distance(+Pos1, +Pos2, ?Distance)
% Manhattan distance between two grid squares
map_distance(p(X,Y),p(X1,Y1), D) :-
  D is abs(X - X1) + abs(Y - Y1).
% Part 1 & 3 + 4 (End)

%%%%%%%%%% agent predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% agent_do_move(+Agent, +To)
% Has constraint that To is map_adjacent to Agent's current position
% Reduces energy by 1 if step is valid
agent_do_move(Agent,To) :-
  nonvar(Agent),
  nonvar(To),
  ailp_internal(agent_energy(Agent, F)),
  F>0,
  %% check p(X,Y) if To is map_adjacent to current position and free
  agent_current_position(Agent,Pos),
  map_adjacent(Pos, To, Obj),
  Obj = empty,!,
  %% send move to server
  p(X,Y) = To,
  do_command([Agent, move, X, Y], _R),
  do_command([Agent, colour, X, Y, yellow]),
  %% move was successful so decrease agent energy
  internal_use_energy(Agent, 1),
  retract(ailp_internal(agent_position(Agent, Pos))),
  assert(ailp_internal(agent_position(Agent, To))).

% agent_do_moves(+Agent, +ListOfMoves)
agent_do_moves(_, []).
agent_do_moves(Agent, [H|T]) :-
  agent_do_move(Agent, H),
  agent_do_moves(Agent,T).

% agent_current_energy(+Agent, -Energy)
agent_current_energy(Agent, Energy) :-
  nonvar(Agent),
  var(Energy),
  ailp_internal(agent_energy(Agent,Energy)),
  ( part(3)   -> atomic_list_concat(['Current energy:',Energy],' ',A),
                 do_command([Agent,console,A])
  ; otherwise -> true
  ).

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
% Part3: fails if oracle already visited by Agent
agent_ask_oracle(Agent, OID, Question, Answer) :-
  nonvar(Agent),
  nonvar(OID),
  ( part(3)   -> \+ ailp_internal(agent_visited_oracle(oscar, OID))
  ; otherwise -> true
  ),
  nonvar(Question),
  var(Answer),
  ( part(3)   -> internal_topup(Emax),
                 Cost is ceiling(Emax/10),
                 ailp_internal(agent_energy(Agent,Energy)),
                 ( Energy>Cost -> agent_current_position(Agent,Pos),
                                  map_adjacent(Pos, AdjPos, OID),
                                  OID = o(_),
                                  internal_object(OID, AdjPos, Options),
                                  member(question(Q)/answer(A),Options),
                                  ( Question=Q -> Answer=A ; Answer='I do not know' ),
                                  atomic_list_concat([Question,Answer],': ',AA),
                                  internal_use_energy(Agent,Cost),
                                  assert(ailp_internal(agent_visited_oracle(oscar, OID)))
                 ; otherwise -> Answer='Sorry, not enough energy',AA=Answer
                 ),
                 do_command([Agent,console,AA])
  ; otherwise -> ( part(1) -> (agent_current_position(Agent,Pos), map_adjacent(Pos, AdjPos, OID))
                 ; part(2) -> true  % ignore agent position for testing
                 ; otherwise -> (write('Unknown part: *'), part(P), write(P), write('*'), nl)
                 ),
                 OID = o(_),
                 internal_object(OID, AdjPos, Options),
                 member(question(Q)/answer(A),Options),
                 ( Question=Q -> Answer=A ; Answer='I do not know' )
  ).

%%%%%%%%%% global predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ailp_start_position(p(1,1)).

ailp_reset :-
  internal_grid_size(N),
  ailp_start_position(p(X0,Y0)),
  % set initial agent state
  retractall(ailp_internal(_)),
  assert(ailp_internal(agent_position(oscar, p(X0,Y0)))),
  internal_topup(Emax),
  assert(ailp_internal(agent_energy(oscar, Emax))),
  ( part(3)   -> init_things(oracle,N/2),
                 init_things(charging_station,N/10),
                 init_things(thing,N*N/4),
                 wp:init_identity  % defined in wp.pl
  ; otherwise -> init_things
  ),
  reset([
    grid_size=N,
    cells=[
      [green, 1,1, N,N]
    ],
    agents=[[oscar, 6, blue, X0,Y0]]
  ]),
  ( part(3)   -> internal_colour_map  % make objects visible at the start
  ; otherwise -> true
  ),
  do_command([oscar, colour, X0, Y0, yellow]).

%%%%%%%%%% Do not query any of the predicates below! %%%%%%%%%%%%%%%%%%%%%%%%%%%
internal_grid_size(20).  % may be changed in testing

internal_topup(Emax):-
  internal_grid_size(N),
  ( part(3)   -> Emax is ceiling(N*N/4)
  ; otherwise -> Emax is N*N/5
  ).

internal_poss_step(p(X,Y), M, p(X1,Y1), I) :-
  member(M, [s,e,n,w]), % try moves in this order
  ( M = s -> X1 =  X,    Y1 is Y+I
  ; M = e -> X1 is X+I,  Y1 =  Y
  ; M = n -> X1 =  X,    Y1 is Y-I
  ; M = w -> X1 is X-I,  Y1 =  Y
  ).

internal_off_board(p(X,Y)) :-
  internal_grid_size(N),
  ( X < 1
  ; X > N
  ; Y < 1
  ; Y > N
  ).

internal_use_energy(Agent, Cost) :-
  nonvar(Agent),
  retract(ailp_internal(agent_energy(Agent, E))),
  E>0, E1 is E - Cost,
  assert(ailp_internal(agent_energy(Agent,E1))),
  ( E1 < 20 -> atomic_list_concat(['WARNING -- Low energy:',E1],' ',A),
         do_command([Agent,console,A])
  ; true
  ).

%%%%%%%%%% Objects on the map %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WP version -- used only for Part 2 of the assignment
internal_object(o(1), p(5,3), [question(link)/answer(Link)]) :-
  part(2),
  wp:ailp_identity(A),
  wp:random_link(A,Link).
%%% Part 1 version
%%% The position and number of these objects may change when testing
% Charging stations
internal_object(c(1), p(1,10), []) :- part(1).
internal_object(c(2), p(10,20), []) :- part(1).
internal_object(c(3), p(20,9), []) :- part(1).
internal_object(c(4), p(9,1), []) :- part(1).
% Oracles that have information
internal_object(o(1), p(5,3), [question(_)/answer(42)]) :- part(1).
% Obstacles (things)
internal_object(t(I), Pos, []) :-
  part(1),
  ailp_internal_thing(I,Pos).
%%% Part 3 version
%%% The position and number of these objects changes every time ailp_reset/0 is called
internal_object(c(I),Pos,[]) :-
  part(3),
  ailp_internal(charging_station(I,Pos)).
%% Oracles that have information
internal_object(o(I),Pos,[question(link)/answer(Link)]):-
  part(3),
  ailp_internal(oracle(I,Pos)),
  wp:ailp_identity(A),
  wp:random_link(A,Link).
%% Obstacles (things)
internal_object(t(I),Pos,[]):-
  part(3),
  ailp_internal(thing(I,Pos)).
%%% Multi version end

% version that makes the object visible on the map
internal_object1(O,Loc,L):-
  internal_object(O,Loc,L),
  ( part(3)   -> true
                 %internal_colour_loc(O,Loc)  % disabled as may cause web client overload
  ; otherwise -> Loc=p(X,Y),
                 ( O=t(_) -> Colour=black   % obstacle
                 ; O=c(_) -> Colour=orange  % charging station
                 ; O=o(_) -> Colour=red     % oracle
                 ),
                 do_command([oscar,colour,X,Y,Colour])
  ).

init_things :-
  retractall(ailp_internal_thing(_,_)),
  assert_internal_things.
  % the following code generates a random set of internal things instead
  %randset(100,9999,S),
  %internal_grid_size(N),
  %internal_things(S,N,1).

internal_things([],_N,_M).
internal_things([Z|Zs],N,M):-
  internal_number2pos(Z,N,Pos),
  ( internal_object(_O,Pos,_L) -> true
  ; ailp_start_position(Pos) -> true
  ; otherwise -> assert(ailp_internal_thing(M,Pos))
  ),
  M1 is M+1,
  internal_things(Zs,N,M1).

internal_number2pos(Z,N,p(X,Y)):-
  K=N*N/5,  % roughly one in five cells is an obstacle
  Z1 is mod(Z,K),
  Z2 is (Z-Z1)/K,
  X is mod(Z1,N) + 1,
  Y is mod(Z2,N) + 1.

% fixed set of obstacles for development, your code will be tested with a
% different set
assert_internal_things:-
  assert(ailp_internal_thing(1,p(7,1))),
  assert(ailp_internal_thing(2,p(11,1))),
  assert(ailp_internal_thing(3,p(12,1))),
  assert(ailp_internal_thing(4,p(8,4))),
  assert(ailp_internal_thing(5,p(5,5))),
  assert(ailp_internal_thing(6,p(7,6))),
  assert(ailp_internal_thing(7,p(3,6))),
  assert(ailp_internal_thing(8,p(2,6))),
  assert(ailp_internal_thing(9,p(8,10))),
  assert(ailp_internal_thing(10,p(8,11))),
  assert(ailp_internal_thing(11,p(19,12))),
  assert(ailp_internal_thing(12,p(11,12))),
  assert(ailp_internal_thing(13,p(2,13))),
  assert(ailp_internal_thing(14,p(13,13))),
  assert(ailp_internal_thing(15,p(7,16))),
  assert(ailp_internal_thing(16,p(6,19))),
  assert(ailp_internal_thing(17,p(13,20))),
  assert(ailp_internal_thing(18,p(18,20))),
  assert(ailp_internal_thing(19,p(13,3))),
  assert(ailp_internal_thing(20,p(1,5))),
  assert(ailp_internal_thing(21,p(17,6))),
  assert(ailp_internal_thing(22,p(19,7))),
  assert(ailp_internal_thing(23,p(7,12))),
  assert(ailp_internal_thing(25,p(2,14))),
  assert(ailp_internal_thing(26,p(20,14))),
  assert(ailp_internal_thing(27,p(5,14))),
  assert(ailp_internal_thing(28,p(9,15))),
  assert(ailp_internal_thing(29,p(16,15))),
  assert(ailp_internal_thing(30,p(5,16))),
  assert(ailp_internal_thing(31,p(19,18))),
  assert(ailp_internal_thing(32,p(7,19))),
  assert(ailp_internal_thing(33,p(6,20))),
  assert(ailp_internal_thing(34,p(11,20))),
  assert(ailp_internal_thing(35,p(13,1))),
  assert(ailp_internal_thing(36,p(17,1))),
  assert(ailp_internal_thing(37,p(6,3))),
  assert(ailp_internal_thing(38,p(12,3))),
  assert(ailp_internal_thing(39,p(3,4))),
  assert(ailp_internal_thing(40,p(5,4))),
  assert(ailp_internal_thing(41,p(10,5))),
  assert(ailp_internal_thing(42,p(1,8))),
  assert(ailp_internal_thing(44,p(19,11))),
  assert(ailp_internal_thing(45,p(1,12))),
  assert(ailp_internal_thing(46,p(19,13))),
  assert(ailp_internal_thing(47,p(8,14))),
  assert(ailp_internal_thing(48,p(19,17))),
  assert(ailp_internal_thing(49,p(5,18))),
  assert(ailp_internal_thing(50,p(4,19))),
  assert(ailp_internal_thing(52,p(7,2))),
  assert(ailp_internal_thing(53,p(2,4))),
  assert(ailp_internal_thing(55,p(6,6))),
  assert(ailp_internal_thing(56,p(3,7))),
  assert(ailp_internal_thing(57,p(14,8))),
  assert(ailp_internal_thing(58,p(16,8))),
  assert(ailp_internal_thing(59,p(18,9))),
  assert(ailp_internal_thing(60,p(4,14))),
  assert(ailp_internal_thing(61,p(20,15))),
  assert(ailp_internal_thing(62,p(18,16))),
  assert(ailp_internal_thing(63,p(8,16))),
  assert(ailp_internal_thing(64,p(13,17))),
  assert(ailp_internal_thing(65,p(15,18))),
  assert(ailp_internal_thing(67,p(15,20))),
  assert(ailp_internal_thing(68,p(2,20))),
  assert(ailp_internal_thing(69,p(6,2))),
  assert(ailp_internal_thing(70,p(14,2))),
  assert(ailp_internal_thing(72,p(9,5))),
  assert(ailp_internal_thing(73,p(10,6))),
  assert(ailp_internal_thing(74,p(14,6))),
  assert(ailp_internal_thing(75,p(5,9))),
  assert(ailp_internal_thing(76,p(6,9))),
  assert(ailp_internal_thing(77,p(14,11))),
  assert(ailp_internal_thing(79,p(8,17))),
  assert(ailp_internal_thing(80,p(10,19))),
  assert(ailp_internal_thing(81,p(18,1))),
  assert(ailp_internal_thing(82,p(3,1))),
  assert(ailp_internal_thing(83,p(16,1))),
  assert(ailp_internal_thing(84,p(14,3))),
  assert(ailp_internal_thing(85,p(18,4))),
  assert(ailp_internal_thing(86,p(19,4))),
  assert(ailp_internal_thing(88,p(15,6))),
  assert(ailp_internal_thing(89,p(7,8))),
  assert(ailp_internal_thing(90,p(4,8))),
  assert(ailp_internal_thing(92,p(15,16))),
  assert(ailp_internal_thing(95,p(2,2))),
  assert(ailp_internal_thing(96,p(15,2))),
  assert(ailp_internal_thing(97,p(9,2))),
  assert(ailp_internal_thing(98,p(12,5))),
  assert(ailp_internal_thing(99,p(3,5))).

/*
 *  Part 3
 */
% agent_check_oracle(+Agent, +OID)
% checks whether oracle already visited by Agent
agent_check_oracle(Agent, OID) :-
  nonvar(Agent),
  nonvar(OID),
  ailp_internal(agent_visited_oracle(oscar, OID)).

init_things(Label,Exp) :-
  K is ceiling(Exp),  % round up if Exp evaluates to a fraction
  KK = 99999,
  randset(K,KK,S),
  internal_grid_size(N),
  internal_things(S,N,Label,1).

internal_things([],_N,_L,_M).
internal_things([Z|Zs],N,Label,M):-
  internal_number2pos(Z,N,Pos),
  Fact =.. [Label,M,Pos],
  ( internal_object(_O,Pos,_L) -> true
  ; ailp_start_position(Pos) -> true
  ; otherwise -> assert(ailp_internal(Fact))
  ),
  M1 is M+1,
  internal_things(Zs,N,Label,M1).

internal_colour_map:-
  internal_object(O,Loc,_),
  internal_colour_loc(O,Loc),
  fail.
internal_colour_map.

internal_colour_loc(O,p(X,Y)):-
  ( O=t(_) -> Colour=black  % obstacle
  ; O=c(_) -> Colour=orange % charging station
  ; O=o(_) -> Colour=red    % oracle
  ),
  do_command([oscar,colour,X,Y,Colour]).
/*
 *  Part 3
 */

/*
 *  Part 4
 * Contains predicates which allow the communication between the agent/client and the server through http post requests.
 * All predicates allowed to be sent to the server are indicated by possible_query/2.
 *
 * Only use predicates exported in module heading in your code!
 */
referee_queries_path('http://127.0.0.1:8000/agent/queries').
%referee_queries_path('http://137.222.102.101:8000/agent/queries').

query_world(Pred, Args):-
  possible_query(Pred,Args),
  referee_queries_path(Path),
  term_to_atom(Args, NewArgs),
  http_post(Path,
    form_data([ pred = Pred,
          args = NewArgs
          ]),
    Reply, []),
  term_to_atom(TermReply,Reply),
  ( TermReply = fail -> fail
  ; otherwise-> Args = TermReply
  ).

possible_query(check_pos, [_Pos, _OID]).                     % ?-query_world( check_pos, [p(1,2), empty])
possible_query(agent_ask_oracle, [_Agent,_OID,_Q,_L]).       % ( agent_ask_oracle, [4, o(3), link, L)
possible_query(agent_current_energy, [_Agent,_E]).           % ( agent_current_energy, [6,E]).
possible_query(agent_current_position, [_Agent,_P]).         % ( agent_current_position, [oscar,P]).
possible_query(agent_topup_energy, [_Agent,_ChargStation]).  % ( agent_topup_energy, [2, c(1)]).
possible_query(agent_check_oracle, [_Agent, _Oracle]).       % ( agent_check_oracle, [9, o(1)]).
possible_query(agent_do_moves, [_Agent, _Path]).             % ( agent_do_moves, [ 1, Path]).
possible_query(internal_leave_game, [_Agent]).               % ( internal_leave_game, [ 2]).
possible_query(internal_join_game, [_Agent]).                % ( agent_join_game, [Agent]).
possible_query(game_status, [_Status]).                      % ( game_status, [stopped]).
possible_query(internal_start_game, []).
possible_query(ailp_reset, []).

join_game(Agent):-
  ( \+query_world(game_status,[running]) ->
    ( my_agent(Agent) -> format('Your agent has already joined the game')
    ; otherwise       -> query_world(internal_join_game, [Agent]),
                         assert(ailp_internal(agent(Agent)))
    )
  ; otherwise                            -> format('Cannot join! Game has already started')
  ).

leave_game:-
  my_agent(Agent),
  retract(ailp_internal(agent(Agent))),
  query_world(internal_leave_game, [Agent]).

start_game:-
  query_world(internal_start_game, []).

my_agent(Agent):-
  ailp_internal(agent(Agent)).

reset_game:-
  query_world(ailp_reset, []).
/*
 *  Part 4
 */

/*
 *  Moved from oscar.pl
 */
%%%%%%%%%% command shell %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
shell :-
  get_input(Input),
  handle_input(Input).

%%% Part 1 & 3 + Part 4
handle_input(Input) :-
  ( Input = setup       -> ( part(4)   -> join_game(_A),handle_input(reset)
                           ; otherwise -> show_response('This command works only with Part 4.'),shell
                           )
  ; Input = status      -> ( part(4)   -> query_world(game_status,[S]),show_response(S),shell
                           ; otherwise -> show_response('This command works only with Part 4.'),shell
                           )

  ; Input = whoami      -> ( part(4)   -> my_agent(A),show_response(A),shell
                           ; otherwise -> show_response('This command works only with Part 4.'),shell
                           )

  ; Input = reset       -> ( part(4)   -> reset_game,start_game,shell
                           ; otherwise -> ailp_reset,shell
                           )
  ; Input = stop        -> true
  ; Input = [H|T]       -> handle_input(H),handle_input(T),shell
  ; callable(Input,G,R) -> ( call(G) -> show_response(R) ; show_response('This failed.') ),shell
  ; otherwise           -> show_response('Unknown command, please try again.'),shell
  ).
%%% Part 1 & 3 + Part 4 (End)

% get input from user
get_input(Input) :-
  write('? '),read(Input).

% show answer to user
%%% Part 1 & 3 + Part 4
show_response(R) :-
  ( part(4)   -> my_agent(Agent)
  ; otherwise -> Agent = oscar
  ),
  ( R=shell(Response)   -> writes('! '),writes(Response),writes(nl)
  ; R=console(Response) -> term_to_atom(Response,A),do_command([Agent,console,A])
  ; R=both(Response)    -> show_response(shell(Response)),show_response(console(Response))
  ; R=agent(Response)   -> term_to_atom(Response,A),do_command([Agent,say,A])
  ; R=[H|T]             -> show_response(H),show_response(T)
  ; R=[]                -> true
  ; otherwise           -> writes(['! ',R])
  ).
%%% Part 1 & 3 + Part 4 (End)

writes(A) :-
  ( A=[]      -> nl
  ; A=nl      -> nl
  ; A=[H|T]   -> writes(H),writes(T)
  ; A=term(T) -> write(T)
  ; otherwise -> write(A)
  ).

% callable(+Command, +Goal, ?Response)
%%% Part 1 & 3
callable(call(G),call(G),G) :- (part(1); part(3)), !.
callable(topup(S),agent_topup_energy(oscar,S),agent(topup)) :- (part(1); part(3)), !.
callable(energy,agent_current_energy(oscar,E),both(current_energy(E))) :- (part(1); part(3)), !.
callable(position,agent_current_position(oscar,P),both(current_position(P))) :- (part(1); part(3)), !.
callable(ask(S,Q),agent_ask_oracle(oscar,S,Q,A),A) :- (part(1); part(3)), !.
callable(Task,user:solve_task(Task,Cost),[console(Task),shell(term(Cost))]) :-
  (part(1); part(3)), !,
  task(Task).
%%% Part 1 & 3 (End)
%%% Part 4
callable(topup(S),(my_agent(Agent),query_world( agent_topup_energy, [Agent,S] )),agent(topup)) :- part(4), !.
callable(energy,(my_agent(Agent),query_world( agent_current_energy, [Agent,E] )),both(current_energy(E))) :- part(4), !.
callable(position,(my_agent(Agent),query_world( agent_current_position, [Agent,P] )),both(current_position(P))) :- part(4), !.
callable(ask(S,Q),(my_agent(Agent),query_world( agent_ask_oracle, [Agent,S,Q,A] )),A) :- part(4), !.
callable(Task,user:solve_task(Task,Cost),[console(Task),shell(term(Cost))]):-
  part(4), !,
  task(Task).
%%% Part 4 (End)

task(go(_Pos)).
task(find(_O)).  % oracle o(N) or charging station c(N)
/*
 *  oscar.pl
 */

 %% Extra predicates
say(Message) :-
  do_command([oscar, say, Message]).
say(Message, Agent) :-
  do_command([Agent, say, Message]).
