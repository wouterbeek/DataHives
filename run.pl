% Standalone startup for DataHives.

% DataHives: initializes prefixes.
% Must appear before DataHives modules are loaded.

:- use_module(library(http/http_path)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- multifile(http:location/3).
:- dynamic(http:location/3).

http:location(dh, root(dh), []).

init_prefixes:-
  Prefix1 = 'http://localhost:8888/',
  
  % dh
  rdf_register_prefix(dh, Prefix1),
  
  % dho
  atomic_concat(Prefix1, 'ontology/', Prefix4),
  rdf_register_prefix(dho, Prefix4),
  
  % dh-stats
  atomic_concat(Prefix1, 'stats/', Prefix5),
  rdf_register_prefix('dh-stats', Prefix5).
:- init_prefixes.



:- use_module(library(http/http_dispatch)).

:- if(current_prolog_flag(argv, ['--debug'])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.

:- dynamic(user:web_module/2).
:- multifile(user:web_module/2).



% plServer

:- use_module(load_project).
:- load_subproject(dh, plServer).

:- use_module(plServer/plServer).
:- use_module(plServer/app_server).
:- use_module(plServer/web_modules). % Web module registration.

:- start_app_server([port(8888)]).



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

http:location(dh_agent, dh('Agent'), []).

:- use_module(dh(agent/dh_agent)).

user:web_module('DH Agents', dh_agent).

:- http_handler(dh_agent(.), dh_agent_rest, [prefix,priority(-1)]).

dh_agent_rest(Request):-
  dh_agent_rest(Request, plServer_style).



% DataHives: Agent Definition

http:location(dh_agent_definition, dh('AgentDefinition'), []).

:- use_module(dh(agent/def/dh_agent_definition)).

user:web_module('DH Agent Definitions', dh_agent_definition).

:- http_handler(
     dh_agent_definition(.),
     dh_agent_definition_rest,
     [prefix,priority(-1)]
   ).

dh_agent_definition_rest(Request):-
  dh_agent_definition_rest(Request, plServer_style).



% DataHives: Graphic

:- use_module(dh(web/dh_agent_graphic)).

user:web_module('DH Agent Graphic', dh_agent_graphic).

:- http_handler(dh_agent(graphic), dh_agent_graphic, []).

dh_agent_graphic(Request):-
  dh_agent_graphic(Request, plServer_style).



% DataHives: Statistics

http:location(dh_stats, dh('Statistics'), []).

:- use_module(dh(stats/dh_stats_web)).

user:web_module('DH Statistics', dh_stats_web).

:- http_handler(dh_stats(.), dh_stats_web, [prefix,priority(-1)]).

dh_stats_web(Request):-
  dh_stats_web(Request, plServer_style).

