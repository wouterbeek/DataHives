:- module(conf_dh, []).

:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).

:- if(\+ current_module(load_project)).
  :- ensure_loaded('../load').
:- endif.



% DataHives

:- use_module(dh_web(dh_web_graph)).

:- use_module(cliopatria(hooks)).
   cliopatria:menu_item(700=places/dh, 'DataHives').

:- http_handler(cliopatria(dh), dh_web_graph, []).

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
