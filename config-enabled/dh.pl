:- module(conf_dh, []).

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

http:location(dh, cliopatria(dh), []).

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

:- http_handler(dh_agent(.), dh_agent, [id(dh_agent),prefix,priority(-1)]).

dh_agent(Request):-
  dh_agent(Request, cliopatria(default)).


% DataHives: Agent Definition

http:location(dh_agent_definition, dh(agent_definition), []).

:- use_module(dh_agent_definition(dh_agent_definition)).

cliopatria:menu_item(600=dh/dh_agent_definition, 'DH Agent Definition').

:- http_handler(
     dh_agent_definition(.),
     dh_agent_definition,
     [id(dh_agent_definition),prefix,priority(-1)]
   ).

dh_agent_definition(Request):-
  dh_agent_definition(Request, cliopatria(default)).


% DataHives: Graph

:- use_module(dh_web(dh_agent_graph)).

cliopatria:menu_item(700=dh/dh_agent_graph, 'DH Graph').

:- http_handler(dh(graph), dh_agent_graph, [id(dh_agent_graph)]).

dh_agent_graph(Request):-
  dh_agent_graph(Request, cliopatria(default)).


% plTabular

:- use_module(plTabular(rdf_tabular)).

:- http_handler(cliopatria(plTabular), rdf_tabular, []).

rdf_tabular(Request):-
  rdf_tabular(Request, plTabular).

:- html_resource(plTabular, [requires([css('plTabular.css')]),virtual(true)]).

user:body(plTabular, Body) -->
  html_requires(plTabular),
  user:body(dh, Body).

