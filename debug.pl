% Debug file for the DataHives project.

% Allow GUI tracer to run in threads.
:- guitracer.

% Debug flags.
:- use_module(library(debug_ext)).
:- debug(dh).
:- debug(dh(message)).

:- [load].


% Population-wide predicates for analytics.
:- use_module(dh(core/dh_population)).

% Allow DataHives graphs and resources to be exported to file.
:- use_module(dh(debug/dh_export)).

% Assert the visum sample RDF graph.

:- use_module(library(rdf/rdf_gc)).
:- use_module(library(rdf/rdf_test)).

:- initialization(init).

init:-
  rdf_test(modeling(visum), G),
  rdf_graph_exclude_from_gc(G).

% Allow tests to be run from the top-level.
:- user:use_module(dh(test/dh_test)).
