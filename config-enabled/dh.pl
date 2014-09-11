:- module(conf_dh, []).

http:location(dh, cliopatria(dh), []).

:- use_module(library(settings)).

:- set_setting_default(http:public_host, 'localhost.localdomain').
:- set_setting_default(http:public_port, setting(http:port)).

:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)). % http:location/3 registrations

:- if(\+ current_module(load_project)).
  :- ensure_loaded('../debug').
  %:- ensure_loaded('../load').
:- endif.

:- use_module(cliopatria(hooks)).

:- multifile(http:location/3).
:- dynamic(http:location/3).

:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).


% DataHives: Home

user:file_search_path(css, dh_web(css)).
user:file_search_path(js, dh_web(js)).

:- use_module(dh_web(dh_web)).

cliopatria:menu_item(600=dh/dh, 'DataHives').

:- http_handler(dh(.), dh, [id(dh),prefix,priority(-10)]).

dh(Request):-
  dh(Request, cliopatria(default)).


% DataHives: Agent

http:location(dh_agent, dh(agent), []).

:- use_module(dh_agent(dh_agent)).

cliopatria:menu_item(600=dh/dh_agent, 'DH Agent').

:- http_handler(dh_agent(.), dh_agent_rest, [id(dh_agent),prefix,priority(-1)]).

dh_agent_rest(Request):-
  dh_agent_rest(Request, cliopatria(default)).


% DataHives: Agent Definition

http:location(dh_agent_definition, dh(agent_definition), []).

:- use_module(dh_agent_definition(dh_agent_definition)).

cliopatria:menu_item(600=dh/dh_agent_definition, 'DH Agent Definition').

:- http_handler(
     dh_agent_definition(.),
     dh_agent_definition_rest,
     [id(dh_agent_definition),prefix,priority(-1)]
   ).

dh_agent_definition_rest(Request):-
  dh_agent_definition_rest(Request, cliopatria(default)).


% DataHives: Graphic

:- use_module(dh_web(dh_agent_graphic)).

cliopatria:menu_item(700=dh/dh_agent_graphic, 'DH Graphic').

:- http_handler(dh(graphic), dh_agent_graphic, [id(dh_agent_graphic)]).

dh_agent_graphic(Request):-
  dh_agent_graphic(Request, cliopatria(default)).


% DataHives: Statistics

http:location(dh_stats, dh(stats), []).

:- use_module(dh_stats(dh_stats_web)).

cliopatria:menu_item(700=dh/dh_stats, 'DH Statistics').

:- http_handler(
     dh_stats(.),
     dh_stats_web,
     [id(dh_stats),prefix,priority(-1)]
   ).

dh_stats_web(Request):-
  dh_stats_web(Request, plServer_style).


% plTabular

:- use_module(plTabular(rdf_tabular)).

:- http_handler(cliopatria(plTabular), rdf_tabular, []).

rdf_tabular(Request):-
  rdf_tabular(Request, plTabular).

:- html_resource(plTabular, [requires([css('plTabular.css')]),virtual(true)]).

user:body(plTabular, Body) -->
  html_requires(plTabular),
  user:body(dh, Body).


% The part of the initialization that requires HTTP handlers to be set.

% Load the agent definitions.
:- ensure_loaded(dh_agent_definition(dh_agent_definition_init)).

