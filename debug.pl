% Debug file for the DataHives project.

% Allow GUI tracer to run in threads.
:- guitracer.


% Debug mode flag.

:- dynamic(user:debug_mode/0).
:- multifile(user:debug_mode/0).
user:debug_mode.


:- [load].


% Debug flag.
:- use_module(library(debug)).
:- debug(dh).


%%%%% Thread monitor.
%%%%:- use_module(library(swi_ide)).
%%%%:- prolog_ide(thread_monitor).


% Population-wide predicates for analytics.
:- use_module(dh_core(dh_population)).


% Allow DataHives graphs and resources to be exported to file.
:- use_module(dh_debug(dh_export)).


% Assert the visum sample RDF graph.

:- use_module(plRdf(rdf_gc)).
:- use_module(plRdf(rdf_script)).

:- initialization(init).

init:-
  assert_visum(Graph),
  rdf_graph_exclude_from_gc(Graph).



% Debug flags

:- use_module(library(debug)).

:- debug(dh_message).
:- debug(real).
:- nodebug(http(request)).

