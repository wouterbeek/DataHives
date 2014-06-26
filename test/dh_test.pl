:- module(
  dh_test,
  [
      dh_test/0,
      dh_test/1, % ?Url:url
      dh_ant_test/0,
      dh_ant_test/1, % ?Url:url
      dh_bee_test/0,
      dh_bee_test/1  % ?Url:url
  ]
).

/** <module> DataHives tests

Simple test predicates for running programs in DataHives.

@author Wouter Beek
@author Baudouin Duthoit
@version 2013/09-2013/10, 2014/02, 2014/04-2014/06
*/

:- use_module(library(www_browser)).

:- use_module(generics(meta_ext)).

:- use_module(plRdf(rdf_gc)).
:- use_module(plRdf(rdf_script)).

:- use_module(dh_agent(dh_agent_ant)).
:- use_module(dh_agent(dh_agent_bee)).
:- use_module(dh_agent(dh_agent_entailment)).
:- use_module(dh_com(dh_edge_weight)).
:- use_module(dh_core(dh_agent)).
:- use_module(dh_core(dh_act)).
:- use_module(dh_core(dh_communicate)).
:- use_module(dh_core(dh_evaluate)).
:- use_module(dh_core(dh_navigate)).
:- use_module(dh_nav(dh_random_lod_walk)).
:- use_module(dh_nav(dh_bee_fly)).
:- use_module(dh_nav(dh_weighted_lod_walk)).
:- use_module(dh_test(dh_test_generics)).

:- dynamic(dh_test:www_open).

:- initialization(assert(dh_test:www_open(false))).



dh_test:-
  assert_visum(Graph),
  rdf_graph_exclude_from_gc(Graph),
  create_agent(
    dh_random_lod_walk(Graph),
    default_action,
    update_edge_count(1),
    default_evaluation,
    Graph
  ).

dh_test(Url):-
  default_goal(random_start_url, Url),
  create_agent(
    dh_random_lod_walk,
    no_action,
    update_edge_count(1),
    no_evaluation,
    Url
  ).


dh_ant_test:-
  assert_visum(Graph),
  rdf_graph_exclude_from_gc(Graph),
  create_agent(
    dh_weighted_lod_walk,
    deductive_action,
    update_edge_count(1),
    evaluate_entailment,
    dh_ant_test,
    Graph
  ).

dh_ant_test(Url):-
  default_goal(random_start_url, Url),
  create_agent(
    dh_weighted_lod_walk,
    deductive_action,
    update_edge_count(1),
    evaluate_entailment,
    dh_ant_test,
    Url
  ).


dh_bee_test:-
  assert_visum(Graph),
  rdf_graph_exclude_from_gc(Graph),
  create_agent(
    dh_bee_lod_fly,
    scout_action,
    update_edge_count(1),
    evaluate_scout,
    dh_bee_test,
    Graph
  ).

dh_bee_test(Url):-
  default_goal(random_start_url, Url),
  create_agent(
    dh_bee_lod_fly,
    scout_action,
    update_edge_count(1),
    evaluate_scout,
    dh_bee_test,
    Url
  ).



% Helpers

www_open:-
  dh_test:www_open(true), !.
www_open:-
  retract(dh_test:www_open(false)), !,
  assert(dh_test:www_open(true)),
  www_open_url('http://localhost:3040/dh/graph').

