:- module(hives_test, []).

/** <module> HIVES

@author Wouter Beek
@version 2013/09
*/

:- use_module(library(plunit)).

:- begin_tests(hives).

:- use_module(datahives(hives)).
:- use_module(datahives(hive_sampling)).
:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(logic(rdf_axiom)).
:- use_module(owl(owl_build)).
:- use_module(rdf(rdf_dataset)).

test(hives, [true]):-
  rdf_assert(rdf:n1, rdf:e12, rdf:n2, g1),
  rdf_assert(rdf:n3, rdf:e34, rdf:n4, g2),
  owl_assert_resource_identity(rdf:n2, rdf:n3, g3),
  owl_assert_resource_identity(rdf:n3, rdf:n5, g3),
  rdf_create_dataset(g1, [g2-g2,g3-g3], DS1),
  create_hive(h1, DS1, _H1),
  
  rdf_assert(rdf:n5, rdf:e54, rdf:n4, g4),
  rdf_assert(rdf:n5, rdf:e56, rdf:n6, g4),
  rdf_create_dataset(g1, [g4-g4], DS2),
  create_hive(h2, DS2, _H2),
  
  register_home_hive(h1),
  
  connect_hives,
  
  rdf_create_graph(stash),
  start_materializer(stash, 10),
  multi(start_sampler(stash, 30), 5).

:- end_tests(hives).

