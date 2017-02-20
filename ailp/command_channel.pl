:- module(command_channel,
  [ start/0,
    stop/0,
    restart/0,
    set_homepage/1,  % +Url_path
    reset/1,         % +Initial_state
    await_result/0,
    await_result/1,  % -Result
    do_command/1,    % Command
    do_command/2     % Command, Result
  ]
).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(www_browser)).
:- use_module(library/game_predicates).

:- dynamic
  command/1,
  result/1,
  homepage/1.

%-----------------------------------------------------------------------------
% REST API WEB SERVER
api_method_commands(_Request) :-
  %% memberchk(method(post), Request),
  findall(C, command(C), CommandList),
  retractall(command(_)),
  retractall(result(_)),
  % write response back to client to enact commands
  format('Content-type: application/json~n~n'),
  current_output(Stream),
  json_write(Stream,
    json([
      commands=CommandList
    ])
  ).

api_method_results(Request) :-
  %% memberchk(method(post), Request),
  get_data_from_request(Request, Params),
  memberchk(results=Results_atom, Params),
  % example of Results:
  %   json([a1=json([a=123, b=456]), a2=json([a=123, b=456]), a3=json([a=123, b=456])]).
  %% memberchk(json([command=_, result=Result
  %% atom_json_term(Results_atom, Results, []),
  %% assert(result(Results)),
  %%   Results = json(R),
  %% assert(result(R)),
  assert(result(Results_atom)),
  %% \+ memberchk([ _ = json([fail = @true]) ], R),
  % write emply json array response back to client
  format('Content-type: application/json~n~n'),
  current_output(Stream),
  json_write(Stream, json([])).

await_result :-
  await_result(_).
await_result(R) :-
  result(R),
  !,
  retractall(result(_)).
await_result(R) :-
  sleep(0.2),
  await_result(R).

do_command(Command) :-
  assert(command(Command)).
do_command(Command, Result) :-
  assert(command(Command)),
  await_result(Result).

answer_query(Request):-
  % get_data_from_request(Request, Data),
  format('Content-type: text/plain~n~n', []),
  http_parameters( Request,
          [ pred(Pred, []),
            args(Args, [])
          ]),
  term_to_atom(Terms,Args),
  ( apply(Pred, Terms) -> term_to_atom(Terms,A), format(A)
  ; otherwise -> format(fail)
  ).

% Requests for files in website's root or any of its subfolders
% need to return static files from the web server's /web/ folder.
% Examples include: /index.html /robots.txt /resources/images/logo.jpg
:- http_handler(root(.), http_reply_from_files('ailp/web', []), [prefix]).

% The following handlers are exceptions to the above default:
:- http_handler('/commands', api_method_commands, []).
:- http_handler('/results', api_method_results, []).
:- http_handler('/agent/queries', answer_query, [prefix]).
%:- http_handler('/agent/commands', get_command, []).

server_host('http://127.0.0.1').
server_port(8000).
server_url(Url) :-
  server_host(Host),
  server_port(Port),
  atomic_list_concat([Host, ':', Port], Url).

%   get_data_from_request(Request+, Data-)
%
%   Merge data from POST (eg. forms) and GET (eg. search) into
%   a single Data list of attribute=value pairs.
%
get_data_from_request(Request, Data) :-
    (
        member(method(post), Request)
    ->  http_read_data(Request, HTTP_POST_Data, [])
    ;   HTTP_POST_Data = []
    ),
    (   memberchk(search(HTTP_GET_Data), Request)
    ->  true
    ;   HTTP_GET_Data = []
    ),
    append(HTTP_GET_Data, HTTP_POST_Data, Data).

homepage('index.html').

set_homepage(Url_path) :-
  retractall(homepage(_)),
  assert(homepage(Url_path)).

start_server :-
  server_port(Port),
  http_server(http_dispatch, [port(Port)]).

stop_server :-
  server_port(Port),
  http_stop_server(Port, []).

open_browser :-
  server_url(Url),
  format('Open in browser? (Y/n)'),
  get_single_char(C),
  ((C = 110 ; C = 78)
   ->  % pressed n or N key, so no
     true
   ;  % pressed any other key (i.e. default to yes)
     homepage(Url_path),
     atomic_list_concat([Url, '/', Url_path], Homepage_url),
     www_open_url(Homepage_url)).

% start the web server
start :-
  start_server ->
  (server_url(Url),
  format('Started web server on ~w~n', [Url])),
  open_browser.

% stop the web server
stop :-
  server_url(Url),
  format('Stopping web server on ~w~n', [Url]),
  stop_server,
  format('Web server stopped.~n').

% restart the web server
restart :-
  server_url(Url),
  format('Restarting web server on ~w~n', [Url]),
  stop_server,
  start_server,
  format('Web server restarted.~n').

% reset the command interpreter to initial state
reset(Initial_state) :-
  retractall(command(_)),
  retractall(result(_)),
  do_command([god, reset, json(Initial_state)], _Result).
