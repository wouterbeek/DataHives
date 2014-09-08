:- module(conf_dh, []).

:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).

:- if(\+ current_module(load_project)).
  :- if(current_prolog_flag(argv, ['--debug'])).
    :- ensure_loaded('../debug').
  :- else.
    :- ensure_loaded('../load').
  :- endif.
:- endif.

:- use_module(cliopatria(hooks)).

:- multifile(http:location/3).
:- multifile(user:file_search_path/2).


% DataHives: home.

http:location(dh, cliopatria(dh), []).

user:file_search_path(css, dh_web(css)).
user:file_search_path(js, dh_web(js)).

:- use_module(dh_web(dh_web)).

cliopatria:menu_item(600=dh/home, 'DataHives').

:- http_handler(root(dh), dh_web, [id(dh),prefix,priority(-1)]).


% DataHives: agent.

:- use_module(dh_agent(dh_agent)).

cliopatria:menu_item(600=dh/agent, 'DH Agent').

:- http_handler(dh(agent), dh_agent, [id(agent)]).

dh_agent(Request):-
  dh_agent(Request, cliopatria(default)).


% DataHives: graph.

:- use_module(dh_web(dh_agent_graph)).

cliopatria:menu_item(700=dh/graph, 'DH Graph').

:- http_handler(dh(graph), dh_agent_graph, [id(graph)]).

dh_agent_graph(Request):-
  dh_agent_graph(Request, cliopatria(default)).


% plTabular

:- use_module(plTabular(rdf_tabular)).

:- http_handler(cliopatria(plTabular), rdf_tabular, []).

rdf_tabular(Request):-
  rdf_tabular(Request, plTabular).

:- html_resource(plTabular, [requires([css('plTabular.css')]),virtual(true)]).

:- multifile(user:body//2).
user:body(plTabular, Body) -->
  html_requires(plTabular),
  user:body(cliopatria(default), Body).

