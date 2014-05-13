:- module(
  dh_test,
  [
    dh_test/0,
    dh_test/1 % ?Url:url
  ]
).

/** <module> DataHives tests

Simple test predicates for running programs in DataHives.

@author Wouter Beek
@version 2013/09-2013/10, 2014/02, 2014/04
*/

:- use_module(library(aggregate)).
:- use_module(library(random)).

:- use_module(rdf(rdf_gc_graph)).
:- use_module(rdf(tests/rdf_script)).

:- use_module(dh_core(dh_agent)).
:- use_module(dh_core(dh_action)).
:- use_module(dh_core(dh_communication)).
:- use_module(dh_core(dh_navigation)).
:- use_module(dh_test(dh_init)).


dh_test:-
  assert_visum(G),
  rdf_graph_exclude_from_gc(G),
  create_agent(
    dh_random_walk,
    default_action,
    update_edge_count,
    G
  ).


dh_test(Url):-
  aggregate_all(
    set(StartUrl),
    start_url(StartUrl),
    StartUrls
  ),
  random_member(Url, StartUrls),
  create_agent(
    dh_random_walk,
    default_action,
    update_edge_count,
    Url
  ).

