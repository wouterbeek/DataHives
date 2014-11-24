:- module(conf_dh, []).

:- use_module(library(settings)).

:- set_setting_default(http:public_host, 'localhost.localdomain').
:- set_setting_default(http:public_port, setting(http:port)).

:- if(\+ current_module(load_project)).
  :- ensure_loaded('../debug').
  %:- ensure_loaded('../load').
:- endif.

:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).

:- use_module(cliopatria(hooks)).

:- multifile(cliopatria:menu_item/2).

:- dynamic(http:location/3).
:- multifile(http:location/3).

:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).

http:location(dh, cliopatria(dh), []).



% DataHives: Home

user:file_search_path(css, dh_web(css)).
user:file_search_path(js, dh_web(js)).

:- use_module(dh(web/dh_web)).

cliopatria:menu_item(600=dh/dh, 'DataHives').

:- http_handler(dh(.), dh, [id(dh),prefix,priority(-10)]).

dh(Request):-
  dh(Request, cliopatria(default)).


% DataHives: Agent

http:location(dh_agent, dh('Agent'), []).

:- use_module(dh(agent/dh_agent)).

cliopatria:menu_item(600=dh/dhAgent, 'DH Agent').

:- http_handler(
     dh_agent(.),
     dh_agent_rest,
     [id(dhAgent),prefix,priority(-1)]
   ).

dh_agent_rest(Request):-
  dh_agent_rest(Request, cliopatria(default)).


% DataHives: Agent Definition

http:location(dh_agent_definition, dh('AgentDefinition'), []).

:- use_module(dh(agent/definition/dh_agent_definition)).

cliopatria:menu_item(600=dh/dhAgentDefinition, 'DH Agent Definition').

:- http_handler(
     dh_agent_definition(.),
     dh_agent_definition_rest,
     [id(dhAgentDefinition),prefix,priority(-1)]
   ).

dh_agent_definition_rest(Request):-
  dh_agent_definition_rest(Request, cliopatria(default)).


% DataHives: Graphic

:- use_module(dh(web/dh_agent_graphic)).

cliopatria:menu_item(600=dh/dhGraphic, 'DH Graphic').

:- http_handler(dh('Graphic'), dh_agent_graphic, [id(dhGraphic)]).

dh_agent_graphic(Request):-
  dh_agent_graphic(Request, cliopatria(default)).


% DataHives: Statistics

http:location(dh_stats, dh('Statistics'), []).

:- use_module(dh(stats/dh_stats_web)).

cliopatria:menu_item(600=dh/dhStatistics, 'DH Statistics').

:- http_handler(
     dh_stats(.),
     dh_stats_web,
     [id(dhStatistics),prefix,priority(-1)]
   ).

dh_stats_web(Request):-
  dh_stats_web(Request, cliopatria(default)).


% plTabular

user:file_search_path(css, plTabular_web(css)).

:- use_module(plTabular(rdf_tabular)).

cliopatria:menu_item(600=dh/plTabular, 'plTabular').

:- http_handler(dh(plTabular), rdf_tabular, [id(plTabular)]).

rdf_tabular(Request):-
  rdf_tabular(Request, plTabular).

:- html_resource(
     css(plTabular),
     [requires([css('plTabular.css')]),virtual(true)]
   ).

user:body(plTabular, Body) -->
  html_requires(css(plTabular)),
  user:body(cliopatria(default), Body).


% The part of the initialization that requires HTTP handlers to be set.

% Load the agent definitions.
:- ensure_loaded(dh(agent/definition/dh_agent_definition_init)).

