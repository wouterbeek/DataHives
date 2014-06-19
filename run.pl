% Standalone startup for DataHives.

:- ensure_loaded(load).

:- use_module(load_project).
:- load_subproject(dh, plServer).



% plServer

:- use_module(plServer(plServer)).
:- use_module(plServer(app_server)).
:- use_module(plServer(web_modules)). % Web module registration.

:- start_app_server([]).



% plTabular

:- use_module(library(http/html_head)).

:- use_module(plTabular(rdf_tabular)).

:- http_handler(root(plTabular), rdf_tabular, []).

:- dynamic(user:web_module/2).
:- multifile(user:web_module/2).
   user:web_module('plTabular', rdf_tabular).

rdf_tabular(Request):-
  rdf_tabular(Request, plTabular).

:- html_resource(plTabular, [requires([css('plTabular.css')]),virtual(true)]).



% DataHives

:- use_module(dh_web(dh_web_graph)).

dh_web_graph(Request):-
  dh_web_graph(Request, app_style).

:- dynamic(user:web_module/2).
:- multifile(user:web_module/2).
   user:web_module('DH Graph', dh_web_graph).

