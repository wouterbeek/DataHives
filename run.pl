% Standalone startup for DataHives.

:- use_module(library(http/http_dispatch)).

:- if(current_prolog_flag(argv, ['--debug'])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.



% plServer

:- use_module(load_project).
:- load_subproject(dh, plServer).

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
  rdf_tabular(Request, plServer_style).

:- html_resource(plTabular, [requires([css('plTabular.css')]),virtual(true)]).



% DataHives: home.

:- multifile(http:location/3).
:- dynamic(http:location/3).
   http:location(dh_web, root(dh), []).

:- multifile(user:file_search_path/2).
   user:file_search_path(css, dh_web(css)).
   user:file_search_path(js, dh_web(js)).

:- dynamic(user:web_module/2).
:- multifile(user:web_module/2).

:- use_module(dh_web(dh_web)).

user:web_module('DH Home', dh_web).

:- http_handler(root(dh), dh_web, [id(dh),prefix,priority(-1)]).

dh_web(Request):-
  dh_web(Request, plServer_style).


% DataHives: agent definitions.

:- use_module(dh_web(dh_web_agent_defs)).

user:web_module('DH Agent defs', dh_web_agent_defs).

:- http_handler(dh_web(agent_defs), dh_web_agent_defs, []).

dh_web_agent_defs(Request):-
  dh_web_agent_defs(Request, plServer_style).


% DataHives: agents.

:- use_module(dh_web(dh_web_agents)).

user:web_module('DH Agents', dh_web_agents).

:- http_handler(dh_web(agents), dh_web_agents, []).

dh_web_agents(Request):-
  dh_web_agents(Request, plServer_style).


% DataHives: graph.

:- use_module(dh_web(dh_web_graph)).

user:web_module('DH Graph', dh_web_graph).

:- http_handler(dh_web(graph), dh_web_graph, []).

dh_web_graph(Request):-
  dh_web_graph(Request, plServer_style).

