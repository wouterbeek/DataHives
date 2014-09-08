% Standalone startup for DataHives.

:- use_module(library(http/http_dispatch)).

:- if(current_prolog_flag(argv, ['--debug'])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.

:- multifile(http:location/3).
:- dynamic(http:location/3).

:- multifile(user:file_search_path/2).

:- dynamic(user:web_module/2).
:- multifile(user:web_module/2).



% plServer

:- use_module(load_project).
:- load_subproject(dh, plServer).

:- use_module(plServer(plServer)).
:- use_module(plServer(app_server)).
:- use_module(plServer(web_modules)). % Web module registration.

:- start_app_server([]).


% jQuery

:- use_module(library(http/html_head)).

:- if(predicate_property(user:debug_mode, visible)).
  :- html_resource(
       js(jquery),
       [requires([js('jquery-debug-2.1.1.js')]),virtual(true)]
     ).
:- else.
  :- html_resource(
       js(jquery),
       [requires([js('jquery-min-2.1.1.js')]),virtual(true)]
     ).
:- endif.


% plTabular

:- use_module(library(http/html_head)).

:- use_module(plTabular(rdf_tabular)).

:- http_handler(root(plTabular), rdf_tabular, []).

user:web_module('plTabular', rdf_tabular).

rdf_tabular(Request):-
  rdf_tabular(Request, plServer_style).

:- html_resource(plTabular, [requires([css('plTabular.css')]),virtual(true)]).


% DataHives: home.

http:location(dh, root(dh), []).

user:file_search_path(css, dh_web(css)).
user:file_search_path(js, dh_web(js)).

/*
:- use_module(dh_web(dh_web)).

user:web_module('DH Home', dh_web).

:- http_handler(dh(.), dh_web, [prefix,priority(-1)]).

dh_web(Request):-
  dh_web(Request, plServer_style).
*/


% DataHives: agents.

http:location(dh_agent, dh(agents), []).

:- use_module(dh_agent(dh_agent)).

user:web_module('DH Agents', dh_agent).

:- http_handler(dh_agent(.), dh_agent, [prefix,priority(-1)]).

dh_agent(Request):-
  dh_agent(Request, plServer_style).


% DataHives: agent definitions.

http:location(dh_agent_definition, dh_agent(definitions), []).

:- use_module(dh_agent(dh_agent_definition)).

user:web_module('DH Agent Definitions', dh_agent_definition).

:- http_handler(
     dh_agent_definition(.),
     dh_agent_definition,
     [prefix,priority(-1)]
   ).

dh_agent_definition(Request):-
  dh_agent_definition(Request, plServer_style).


% DataHives: graph.

:- use_module(dh_web(dh_agent_graph)).

user:web_module('DH Agent Graph', dh_agent_graph).

:- http_handler(dh_agent(graph), dh_agent_graph, []).

dh_agent_graph(Request):-
  dh_agent_graph(Request, plServer_style).


% The part of the initialization that requires HTTP handlers to be set.

% Load the agent definitions.
:- ensure_loaded(dh_agent(dh_agent_init)).

