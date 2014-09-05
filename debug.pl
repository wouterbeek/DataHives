% Debug file for the DataHives project.

:- [load].


% Debug flag.
:- use_module(library(debug)).
:- debug(dh).

% Thread monitor.
:- use_module(library(swi_ide)).
:- prolog_ide(thread_monitor).


% Population-wide predicates for analytics.
:- use_module(dh_core(dh_population)).


% Allow DataHives graphs and resources to be exported to file.
:- use_module(dh_debug(dh_export)).


:- dynamic(user:debug_mode/0).
:- multifile(user:debug_mode/0).
:- assert(user:debug_mode).


:- use_module(plRdf(rdf_gc)).
:- use_module(plRdf(rdf_script)).

:- initialization(dh_deb_init).

dh_deb_init:-
  assert_visum(Graph),
  rdf_graph_exclude_from_gc(Graph).

