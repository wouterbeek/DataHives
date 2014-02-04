:- module(
  dh_test,
  [
    test/0,
    test/1, % +IRI:iri
%    test0/0,
    test1/0,
    test2/0
  ]
).

/** <module> DataHives tests

Simple test predicates for running programs in DataHives.

@author Wouter Beek
@version 2013/09-2013/10, 2014/02
*/

:- use_module(dcg(dcg_generic)).
:- use_module(dh(dh_network)).
:- use_module(dh(dh_program)).
:- use_module(dh(dh_term_check)).
:- use_module(dh(dh_traversal)).
:- use_module(dh(dh_triple_count)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module('LOD'(cache_it)).
:- use_module('LOD'('LOD_query')).
:- use_module('SPARQL'('SPARQL_find')).
:- use_module(owl(owl_build)).
:- use_module(rdf(rdf_dataset)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf_reasoning(rdf_mat)).



test:-
  test('http://dbpedia.org/resource/Monkey').

test(IRI1):-
  'SPARQL_find'(dbpedia, IRI1, IRI2),
  run('LOD_walk', some_goal, IRI2).

:- meta_predicate(run(3,3,+)).
run(Travel, Goal, From):-
  call(Travel, From, _, Propositions),
  random_member([From,Link,To], Propositions),
  call(Goal, From, Link, To),
  run(Travel, Goal, To).

some_goal(From, Link, To):-
  dcg_with_output_to(user_output, rdf_triple_name(From, Link, To)),
  nl(user_output),
  flush_output(user_output).

% SPARQL end point.
'LOD_walk'(IRI, Triples):-
  uri_components(Resource, uri_components(_, Domain, _, _, _)),
  'SPARQL_current_remote_domain'(Remote, Domain), !,
  phrase(
    'SPARQL_formulate'(
      _,
      _,
      [],
      select,
      true,
      [p,o],
      [rdf(iri(Resource), var(p), var(o))],
      inf,
      _
    ),
    Query
  ),
  'SPARQL_query'(Remote, Query, _VarNames, Rows),

/*
test0:-
  rdf_assert(rdf:n1, rdf:e12, rdf:n2, g1),
  rdf_assert(rdf:n3, rdf:e34, rdf:n4, g2),
  owl_assert_resource_identity(rdf:n2, rdf:n3, g3),
  owl_assert_resource_identity(rdf:n3, rdf:n5, g3),
  rdf_create_dataset(g1, [g2-g2,g3-g3], DS1),
  create_hive(h1, DS1, _H1),

  rdf_assert(rdf:n5, rdf:e54, rdf:n4, g4),
  rdf_assert(rdf:n5, rdf:e56, rdf:n6, g4),
  rdf_create_dataset(g1, [g4-g4], DS2),
  create_hive(h2, DS2, _H2).
*/

test1:-
  test12,
  start_programs(
    triple_count,
    5,
    dh_network:random_initial_state,
    dh_triple_count:dh_triple_count,
    dh_traversal:next_triple_random,
    1,
    false
  ).

test2:-
  test12,
  start_programs(
    termcheck,
    8,
    dh_network:random_initial_state,
    dh_term_check:dh_term_check,
    dh_traversal:next_triple_random,
    0,
    false
  ).

test12:-
  absolute_file_name(
    data('places-5'),
    File1,
    [access(read),file_type(turtle)]
  ),
  rdf_load([mime(text/turtle)], 'places-5', File1),
  rdf_create_dataset('places-5', [], DS1),
  create_hive(h1, DS1, _H1),

  absolute_file_name(
    data(foaf),
    File2,
    [access(read),file_type(rdf)]
  ),
  rdf_load([], foaf, File2),
  rdf_create_dataset(foaf, [], DS2),
  create_hive(h2, DS2, _H2),

  register_home_hive(h1),
  connect_hives([h1,h2]),
  
  rdf_create_graph(stash),
  start_materializer(stash, se, 60).

