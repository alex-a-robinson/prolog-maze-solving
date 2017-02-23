% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)

find_identity(A) :-
  % Initially all actors are suspects
  findall(PotentialActor, actor(PotentialActor), PotentialActors),
  find_identity(A, PotentialActors),!.

find_identity(Actor, [Actor]). % Return when we have one answer
find_identity(Actor, PotentialActors) :- 
  agent_ask_oracle(oscar, o(1), link, Link),
  actors_with_link(Link, PotentialActors, [], ReducedPotentialActors),
  find_identity(Actor, ReducedPotentialActors).

% Finds list of actors containg Link from Actors list, effectivly map reduce
actors_with_link(_, [], ActorsWithLink, ActorsWithLink). % Once list exhausted
actors_with_link(Link, [Actor|Actors], WorkingActorsWithLink, ActorsWithLink) :-
  wp(Actor, WT),
  findall(X, wt_link(WT, X), Links),
  ( memberchk(Link, Links) -> actors_with_link(Link, Actors, [Actor|WorkingActorsWithLink], ActorsWithLink)
  ; otherwise              -> actors_with_link(Link, Actors, WorkingActorsWithLink, ActorsWithLink)
  ).
