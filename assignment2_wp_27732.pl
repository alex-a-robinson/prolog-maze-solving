% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  findall(PotentialActor, actor(PotentialActor), PotentialActors),
  find_identity(A, PotentialActors).

find_identity(A, [A]).
find_identity(A, PotentialActors):- 
  agent_ask_oracle(oscar,o(1),link,Link),
  test_actors(Link, PotentialActors, [], Final),
  find_identity(A, Final).

test_actors(_, [], Final, Final).
test_actors(Link, [Actor|PAs], PAsWithLink, Final) :-
  wp(Actor,WT),
  findall(X, wt_link(WT, X), Links),
  ( memberchk(Link, Links) -> test_actors(Link, PAs, [Actor|PAsWithLink], Final)
  ; otherwise -> test_actors(Link, PAs, PAsWithLink, Final)
  ).
