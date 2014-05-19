:- module(
  dh_test,
  [
      dh_test/0,
      dh_test/1, % ?Url:url
      dh_supervised_test/0,
      dh_supervised_test/1 % ?Url:url

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

:- dynamic(start_url/2).

start_url('http://dbpedia.org/resource/Banana').
start_url('http://rdf.freebase.com/ns/m.08pbxl').



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



dh_supervised_test:-
  assert_visum(G),
  rdf_graph_exclude_from_gc(G),
  create_agent(
    dh_supervised_walk,
    supervised_action,
    update_edge_count,
    G
  ).

dh_supervised_test(Url):-
  aggregate_all(
    set(StartUrl),
    start_url(StartUrl),
    StartUrls
  ),
  random_member(Url, StartUrls),
  create_agent(
    dh_supervised_walk,
    supervised_action,
    update_edge_count,
    Url
  ).











