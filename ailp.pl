#!/usr/bin/env swipl
/*
 *  ailp.pl
 *
 *  AI and Logic Programming assignment runner.
 *  Loads and runs specified assignmentN_library.pl file from
 *  assignmentN folder.
 */

find_submission(Assignment, Submission) :-
  directory_files('./', Files),
  ( Assignment='assignment1' -> find_file('assignment1_', Files, Submission)
  ; Assignment='oscar'       -> find_file('assignment2_', Files, Submission)
  ; Assignment='wp'          -> find_file('assignment2_wp_', Files, Submission)
  ).
find_file(_, [], _) :- fail.
find_file(Prefix, [F|Files], Submission) :-
  ( check_file(Prefix, F) -> F=Submission
  ; otherwise -> find_file(Prefix, Files, Submission)
  ).
check_file(Prefix, File) :-
  atom_concat(Basename, '.pl', File),
  atom_concat(Prefix, Candidate, Basename),
  atom_number(Candidate, _).

translate_input_options(Args, Assignment_name, Part) :-
  ( Args=['assignment1'|[]]         -> Assignment_name='assignment1',Part=0
  ; Args=['assignment2','part1'|[]] -> Assignment_name='oscar',Part=1
  ; Args=['assignment2','part2'|[]] -> Assignment_name='wp',Part=2
  ; Args=['assignment2','part3'|[]] -> Assignment_name='oscar',Part=3
  ; Args=['assignment2','part4'|[]] -> Assignment_name='oscar',Part=4
  ; otherwise                       -> fail
  ).

read_in_choice(Assignment_name, Part) :-
  nl, write('Please input `> assignment_name [assignment_part]`'),nl,
  write('  e.g. `assignment1` or `assignment2 part2`'),nl,
  write('> '), read_line_to_codes(user_input, InputCodes),
  atom_codes(InputAtom, InputCodes),
  atomic_list_concat(InputList, ' ', InputAtom),
  ( translate_input_options(InputList, Assignment_name, Part) -> true
  ; read_in_choice(Assignment_name, Part)
  ).

:- dynamic
     user:prolog_file_type/2,
     user:file_search_path/2.

:- multifile
     user:prolog_file_type/2,
     user:file_search_path/2.

:-  % parse command line arguments
    current_prolog_flag(argv, Args),
    % take first argument to be the name of the assignment folder
    ( translate_input_options(Args, Assignment_name, Part) -> true
    ; % missing argument, so display syntax and halt
       nl,
       write('Syntax:'),nl,
       nl,
       write('  ./ailp.pl <assignment_name> [part]'),nl,
       write('e.g. ./ailp.pl assignment1'),nl,
       write('or'),nl,
       write('e.g. ./ailp.pl assignment2 part1'),nl,
       nl,
       write('~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+'),nl,
       nl,
       read_in_choice(Assignment_name, Part)
    ),
    assert(assignment(Assignment_name)),
    assert(sub_assignment(Part)).

:-  % get assignment details
    assignment(Assignment_name),
    sub_assignment(Assignment_part),
    atom_concat(Assignment_name, '_library', Assignment_library_name),
    %
    % define a module path assignment_root()
    prolog_load_context(directory, Sys),
    (\+ user:file_search_path(assignment_root, Sys)
    ->  asserta(user:file_search_path(assignment_root, Sys))
    ),
    %
    % define our own ailp path assignment_ailp()
    atom_concat(Sys, '/ailp', Ailp),
    (\+ user:file_search_path(assignment_ailp, Ailp)
    ->  asserta(user:file_search_path(assignment_ailp, Ailp))
    ),
    %
    % define our own Library path assignment_library()
    atom_concat(Ailp, '/library', Lib),
    (\+ user:file_search_path(assignment_library, Lib)
    ->  asserta(user:file_search_path(assignment_library, Lib))
    ),
    %
    % find candidate submission
    find_submission(Assignment_name, Submission),
    %
    % load files
    load_files(
      [ %assignment_ailp(command_channel),
        %assignment_library(Assignment_library_name),
        assignment_root(Submission)
      ],
      [ silent(true)
      ]
    ),
    %
    % load files and set up dependencies
    ( Assignment_part=0  -> use_module(assignment_library(Assignment_library_name))
    ; Assignment_part=1  -> use_module(assignment_library(Assignment_library_name), [map_adjacent/3,map_distance/3,agent_do_move/2,agent_do_moves/2,agent_current_energy/2,agent_current_position/2,agent_topup_energy/2,agent_ask_oracle/4,ailp_reset/0,ailp_start_position/1,part/1,shell/0,start/0,stop/0,say/1])
    ; Assignment_part=2 -> use_module(assignment_library(Assignment_library_name), [agent_ask_oracle/4,part/1,wp/1,wp/2,wt_link/2,actor/1,link/1,init_identity/0,test/0]),
                           retract(part(1)), assertz(part(2))
    ; Assignment_part=3 -> use_module(assignment_library('wp_library'), except([part/1,agent_ask_oracle/4])),
                           use_module(assignment_library(Assignment_library_name), [map_adjacent/3,map_distance/3,agent_do_move/2,agent_do_moves/2,agent_current_energy/2,agent_current_position/2,agent_topup_energy/2,agent_ask_oracle/4,ailp_reset/0,ailp_start_position/1,agent_check_oracle/2,part/1,shell/0,start/0,stop/0,say/1]),
                           find_submission('wp', WpSubmission),
                           load_files([assignment_root(WpSubmission)], [silent(true)]),
                           retract(part(1)), assertz(part(3))
    ; Assignment_part=4 -> use_module(assignment_library('wp_library'), except([part/1,agent_ask_oracle/4])),
                           use_module(assignment_library(Assignment_library_name), [part/1,shell/0,start/0,stop/0,query_world/2,possible_query/2,my_agent/1,leave_game/0,join_game/1,start_game/0,reset_game/0,map_adjacent/3,map_distance/3]),
                           find_submission('wp', WpSubmission),
                           load_files([assignment_root(WpSubmission)], [silent(true)]),
                           retract(part(1)), assertz(part(4))
    ; otherwise         -> true
    ).
