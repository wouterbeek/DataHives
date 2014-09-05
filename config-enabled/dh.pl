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



% DataHives: home.

:- multifile(http:location/3).
   http:location(dh_web, cliopatria(dh_web), []).

:- multifile(user:file_search_path/2).
   user:file_search_path(css, dh_web(css)).
   user:file_search_path(js, dh_web(js)).

:- use_module(dh_web(dh_web)).

cliopatria:menu_item(600=dh/home, 'DataHives').

:- http_handler(root(dh), dh_web, [id(dh),prefix,priority(-1)]).


% DataHives: agent.

:- use_module(dh_web(dh_web_agents)).

cliopatria:menu_item(600=dh/agent, 'DH Agents').

:- http_handler(dh_web(agents), dh_web_agents, [id(agents)]).

dh_web_agents(Request):-
  dh_web_agents(Request, cliopatria(default)).


% DataHives: graph.

:- use_module(dh_web(dh_web_graph)).

cliopatria:menu_item(700=dh/graph, 'DH Graph').

:- http_handler(dh_web(graph), dh_web_graph, [id(graph)]).

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

