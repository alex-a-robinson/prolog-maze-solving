/*
 *  assignment1_module.pl
 *  assignment-specific program NOT to be edited by students.
 */

:- module(assignment1,
  [ ailp_show_move/2,      % +Old_pos, +New_pos
    ailp_start_position/1, % binds with starting position p(X,Y)
    ailp_show_complete/0,  %
    ailp_grid_size/1,      % -Size
    headless/1,            % enable/disable visualisations
    %%% moved from assignment1.pl file %%%
    complete/1,
    new_pos/3,
    m/1,
    next/1,
    %%% re-exported from command_channel.pl %%%
    reset/0,
    start/0,
    stop/0
  ]
).

:- use_module('../command_channel').
:- set_homepage('mower.html').

% If headless(true) disable all do_command predicates
:- dynamic headless/1.
headless(false).

:- dynamic ailp_grid_size/1.
ailp_grid_size(4).

% Commands:
%  [AgentId, say, Atom]
%  [AgentId, console, Atom]
%  [AgentId, go, Dir]
%  [AgentId, move, X,Y]
%  [AgentId, colour, X,Y,Colour]
%  [god, reset, Initial_state] // asserted by reset/0 to initialise game
%                               // world in web page

ailp_show_move(p(X0,Y0),p(X1,Y1)) :-
  headless(H),
  ( H -> true
  ; (do_command([mower, colour, X0, Y0, lighter]),
     do_command([mower, colour, X1, Y1, lighter]),
     do_command([mower, move, X1, Y1], _Result)
    )
  ).
  % term_to_atom(Result, A), do_command([mower, console, A]).
  % could succeed or fail here depending on legality of attempted move
  % (indciated by 'fail= @true' in R)

ailp_show_complete :-
  headless(H),
  ( H -> true
  ; do_command([mower, say, 'Finished!'], _R)
  ).

% can change to use either start_position/1 or
% start_position_personal/1
ailp_start_position(P) :- start_position_personal(P).

start_position(p(X,Y)) :- X = 1, Y = 1.

% X position is mod(candidatenumber/gridwidth)
% Y position is mod(second digit/gridwidth)
start_position_personal(p(X,Y)):-
  user:candidate_number(Z),
  ailp_grid_size(N),
  X is mod(Z,N) + 1,
  number_codes(Z,[_A|[Y1|_B]]),
  Y2 is Y1 - 48,
  Y is mod(Y2,N) + 1.

reset :-
  ailp_grid_size(N),
  ailp_start_position(p(X,Y)),
  reset([
    grid_size=N,
    cells=[
      [forestgreen, 1,1, N,N]
    ],
    agents=[
      [mower, 6, royalblue, X,Y]
    ]
  ]),
  headless(H),
  ( H -> true
  ;      do_command([mower, colour, X,Y, lighter])
  ).

/*
 *  assignment1.pl
 */
complete(L) :-
  ailp_grid_size(N),
  N2 is N * N,
  length(L,N2),
  ailp_show_complete.

new_pos(p(X,Y), M, p(X1,Y1)) :-
  ( M = s -> X1 =  X,    Y1 is Y+1
  ; M = n -> X1 =  X,    Y1 is Y-1
  ; M = e -> X1 is X+1,  Y1 =  Y
  ; M = w -> X1 is X-1,  Y1 =  Y
  ),
  X1 >= 1, Y1 >=1,
  ailp_grid_size(N),
  X1 =< N, Y1 =< N.

m(s).
m(e).
m(w).
m(n).

next(L) :-
  ailp_start_position(p(X,Y)),
  next(p(X,Y),L).
% P: current position
% L: path taken by agent
next(P,L) :-
  next(P,[P],Ps),
  reverse(Ps,L).
next(_,Ps,Ps) :- complete(Ps).
next(P,Ps,R) :-
  m(M),
  new_pos(P,M,P1),
  \+ memberchk(P1,Ps),
  ailp_show_move(P,P1),
  term_to_atom([P1|Ps],PsA),
  headless(H),
  ( H -> true
  ;      do_command([mower,console,PsA],_R)
  ),
  next(P1,[P1|Ps],R).
/*
 *      assignment1.pl
 */
