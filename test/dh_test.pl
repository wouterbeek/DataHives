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
@version 2013/09-2013/10, 2014/02, 2014/04-2014/05
*/

:- use_module(library(aggregate)).
:- use_module(library(random)).
:- use_module(library(www_browser)).

:- use_module(plRdf(rdf_gc)).
:- use_module(plRdf(rdf_script)).

:- use_module(dh_core(dh_agent)).
:- use_module(dh_core(dh_action)).
:- use_module(dh_core(dh_communication)).
:- use_module(dh_core(dh_lod_walk_random)).
:- use_module(dh_core(dh_lod_walk_supervised)).
:- use_module(dh_core(dh_navigation)).
:- use_module(dh_test(dh_init)).

:- dynamic(dh_test:www_open).
:- initialization(assert(dh_test:www_open(false))).



dh_test:-
  assert_visum(G),
  rdf_graph_exclude_from_gc(G),
  create_agent(
    dh_lod_walk_random,
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
    dh_lod_walk_random,
    default_action,
    update_edge_count,
    Url
  ),
  www_open.

www_open:-
  dh_test:www_open(true), !.
www_open:-
  retract(dh_test:www_open(false)), !,
  assert(dh_test:www_open(true)),
  www_open_url('http://localhost:3040/dh/graph').


dh_supervised_test:-
  assert_visum(G),
  rdf_graph_exclude_from_gc(G),
  create_agent(
    dh_lod_walk_supervised,
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
    dh_lod_walk_supervised,
    supervised_action,
    update_edge_count,
    Url
  ).

