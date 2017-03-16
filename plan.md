find_charging_station()
need_charge()
closest_charging_station()
find_oracle()

charging_stations = [(id, pos)]
Initially:
  [(1, NONE), (2, NONE)]

unqueried_oracles = [(id, pos)], UnvisitedWithKnownPos = [], Univitied = [1,...10]
Initially:
  [(1, None, 0), ...]
Find next closest oracle:
  - Look at all oracles with a pos
  - Pick id of minimum Manhattan distance to pos from current pos
  - If no oracles with pos
    - Pick first  oracle

Find(type(id)):
 - If see another thing keep track of it
 - If its a charging station then charge
 - If its a oracle and in unqueried_oracles then query unless going to a charging station

1. Find all charging stations
  1.1 Update seen oracles
2. Find next closest oracle / first seen one
  2.1 If no known oracles, go to next in unvisited list\




3. Check if task is feasible and update accordingly
    - if not feasible go to closest charging station
    - if feasible do task
    RETURN task;
4. Execute move
  4.1 if nothing on the way, return the object found

5. Check action to do at object
    -if object is oracle
      - if task is find or go to oracle, query
      - else continue with no action and update
                if: oracle is a memberchk of unvisited oracles || known_Oracle_positions
                    -> update lists
    - if object is charging station then charge


6. Check if identity found and quit else and reevaluate (Go To 3.)


---------- BUGS FOUND-----------

- Found oracle which has already been queried but then returns false and stops executing



----------TODO----------------

  % No oracles or charging stations found on the way




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%%%%%%%%%%%%%%%%%%%% Part 4 %%%%%%%%%%%%%%%%%%%%%%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  2) Crashes after all charging stations have been visited and visiting first oracle
  3) When obstacle in path returns false;

  4) bug: task is find(o) or go(Pos)
  5) take into account 1 charging station
------------------------ NEXT ITERATION --------------------------
          Actor Found +_35410
          true.
