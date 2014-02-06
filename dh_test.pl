:- module(
  dh_test,
  [
    dh_test/1, % ?URL
    dh_type_check/1, % ?URL
    dh_lit_lang/1 % ?URL
  ]
).

/** <module> DataHives tests

Simple test predicates for running programs in DataHives.

@author Wouter Beek
@version 2013/09-2013/10, 2014/02
*/

:- use_module(dcg(dcg_generic)).
:- use_module(dh(dh)).
:- use_module(dh(dh_network)).
:- use_module(dh(dh_program)).
:- use_module(dh(dh_walk)). % Meta-argument.
:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_dataset)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf_reasoning(rdf_mat)).
:- use_module(server(web_threads)). % Load the Web-interface for threads.

default_url('http://dbpedia.org/resource/Banana').

:- debug(dh).
:- debug(dh_walk).



% Use case 0

dh_test(URL1):-
  default_url(URL0),
  default(URL1, URL0, URL2),
  init_agent(
    dh_random_walk,
    some_action,
    some_communication, %STUB
    URL2
  ).



% Use case 1: verify the well-formedness of IRIs

dh_type_check(URL1):-
  default_url(URL0),
  default(URL1, URL0, URL2),
  init_agent(
    dh_random_walk,
    type_check,
    some_communication, %STUB
    URL2
  ).

type_check(_, _, _, _, To):-
  type_check(To).

:- dynamic(malformed_uri/1).
type_check(To):-
  rdf_is_bnode(To), !.
type_check(To):-
  rdf_is_literal(To), !.
type_check(To):-
  uri_components(To, _), !.
type_check(To):-
  malformed_uri(To), !.
type_check(To):-
  assert(malformed_uri(To)).



% Use case 2: count languages and datatypes used in literals.

dh_lit_lang(URL1):-
  default_url(URL0),
  default(URL1, URL0, URL2),
  init_agent(
    dh_random_walk,
    lit_lang,
    some_communication, %STUB
    URL2
  ).

lit_lang(Alias, _, _, _, To):-
  flag(Alias, N, N + 1),
  lit_lang(Alias, To).

:- dynamic(literal_language/3).
lit_lang(Alias, literal(lang(Lang,_))):- !,
  increment_literal_language(Alias, Lang).
lit_lang(Alias, literal(type(Datatype1,_))):- !,
  dcg_with_output_to(atom(Datatype2), rdf_term_name(Datatype1)),
  increment_literal_language(Alias, Datatype2).
lit_lang(Alias, literal(_)):- !,
  increment_literal_language(Alias, plain).
lit_lang(_, _).

increment_literal_language(Alias, Lang):-
  retract(literal_language(Alias, Lang, N1)),
  N2 is N1 + 1,
  assert(literal_language(Alias, Lang, N2)).
increment_literal_language(Alias, Lang):-
  assert(literal_language(Alias, Lang, 1)).



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
*/

