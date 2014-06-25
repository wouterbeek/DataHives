% Standalone startup for DataHives.

:- if(current_prolog_flag(argv, ['--debug'])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.

:- use_module(load_project).
:- load_subproject(dh, plServer).



% plServer

:- use_module(plServer(plServer)).
:- use_module(plServer(app_server)).
:- use_module(plServer(web_modules)). % Web module registration.

:- start_app_server([]).



% plTabular

:- use_module(library(http/html_head)).

:- use_module(plTabular(rdf_tabular)).

:- http_handler(root(plTabular), rdf_tabular, []).

:- dynamic(user:web_module/2).
:- multifile(user:web_module/2).
   user:web_module('plTabular', rdf_tabular).

rdf_tabular(Request):-
  rdf_tabular(Request, app_style).

:- html_resource(plTabular, [requires([css('plTabular.css')]),virtual(true)]).



% DataHives

:- multifile(http:location/3).
:- dynamic(http:location/3).
   http:location(dh_web, root(dh), []).

:- dynamic(user:web_module/2).
:- multifile(user:web_module/2).


% DataHives: Agents.

:- use_module(dh_web(dh_web_agent)).

user:web_module('DH Agent', dh_web_agent).

:- http_handler(dh_web(agent), dh_web_agent, []).

dh_web_agent(Request):-
  dh_web_agent(Request, app_style).


% DataHives: Graph.

:- use_module(dh_web(dh_web_graph)).

user:web_module('DH Graph', dh_web_graph).

:- http_handler(dh_web(graph), dh_web_graph, []).

dh_web_graph(Request):-
  dh_web_graph(Request, app_style).

