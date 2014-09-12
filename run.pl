% Standalone startup for DataHives.

:- use_module(library(http/http_dispatch)).

:- if(current_prolog_flag(argv, ['--debug'])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.

:- multifile(http:location/3).
:- dynamic(http:location/3).

:- dynamic(user:web_module/2).
:- multifile(user:web_module/2).



% plServer

:- use_module(load_project).
:- load_subproject(dh, plServer).

:- use_module(plServer(plServer)).
:- use_module(plServer(app_server)).
:- use_module(plServer(web_modules)). % Web module registration.

:- start_app_server([]).


% DataHives: home.

http:location(dh, root(dh), []).


% plTabular

:- use_module(library(http/html_head)).

:- use_module(plTabular(rdf_tabular)).

:- http_handler(root(plTabular), rdf_tabular, []).

user:web_module('plTabular', rdf_tabular).

rdf_tabular(Request):-
  rdf_tabular(Request, plServer_style).

:- html_resource(
     css(plTabular),
     [requires([css('plTabular.css')]),virtual(true)]
   ).


% DataHives: Agent

http:location(dh_agent, dh(agent), []).

:- use_module(dh_agent(dh_agent)).

user:web_module('DH Agents', dh_agent).

:- http_handler(dh_agent(.), dh_agent, [prefix,priority(-1)]).

dh_agent(Request):-
  dh_agent(Request, plServer_style).


% DataHives: Agent Definition

http:location(dh_agent_definition, dh(agent_definition), []).

:- use_module(dh_agent_definition(dh_agent_definition)).

user:web_module('DH Agent Definitions', dh_agent_definition).

:- http_handler(
     dh_agent_definition(.),
     dh_agent_definition,
     [prefix,priority(-1)]
   ).

dh_agent_definition(Request):-
  dh_agent_definition(Request, plServer_style).


% DataHives: Graphic

:- use_module(dh_web(dh_agent_graphic)).

user:web_module('DH Agent Graphic', dh_agent_graphic).

:- http_handler(dh_agent(graphic), dh_agent_graphic, []).

dh_agent_graphic(Request):-
  dh_agent_graphic(Request, plServer_style).


% DataHives: Statistics

http:location(dh_stats, dh(stats), []).

:- use_module(dh_stats(dh_stats_web)).

user:web_module('DH Statistics', dh_stats_web).

:- http_handler(dh_stats(.), dh_stats_web, [prefix,priority(-1)]).

dh_stats_web(Request):-
  dh_stats_web(Request, plServer_style).


% The part of the initialization that requires HTTP handlers to be set.

% Load the agent definitions.
:- ensure_loaded(dh_agent_definition(dh_agent_definition_init)).

