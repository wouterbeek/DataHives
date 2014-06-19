:- module(conf_dh, []).

:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).

:- if(\+ current_module(load_project)).
  :- ensure_loaded('../load').
:- endif.

:- use_module(cliopatria(hooks)).



% DataHives: Agent.

:- use_module(dh_web(dh_web_agent)).

cliopatria:menu_item(600=dh/agent, 'DH Agent').

:- http_handler(cliopatria(dh/agent), dh_web_agent, [id(agent)]).

dh_web_agent(Request):-
  dh_web_agent(Request, cliopatria(default)).


% DataHives: Graph.

:- use_module(dh_web(dh_web_graph)).

cliopatria:menu_item(700=dh/graph, 'DH Graph').

:- http_handler(cliopatria(dh/graph), dh_web_graph, [id(graph)]).

dh_web_graph(Request):-
  dh_web_graph(Request, cliopatria(default)).



% plTabular

:- use_module(plTabular(rdf_tabular)).
rdf_tabular(Request):-
  rdf_tabular(Request, plTabular).

:- http_handler(cliopatria(plTabular), rdf_tabular, []).

:- html_resource(plTabular, [requires([css('plTabular.css')]),virtual(true)]).

:- multifile(user:body//2).
user:body(plTabular, Body) -->
  html_requires(plTabular),
  user:body(cliopatria(default), Body).
