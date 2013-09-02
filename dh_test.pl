:- module(
  dh_test,
  [
    test1/0,
    test2/0
  ]
).

/** <module> HIVES

@author Wouter Beek
@version 2013/09
*/

:- use_module(dh(dh)).
:- use_module(dh(dh_net)).
:- use_module(library(plunit)).
:- use_module(library(semweb/rdf_db)).
:- use_module(owl(owl_build)).
:- use_module(rdf(rdf_dataset)).
:- use_module(rdf(rdf_serial)).



test1:-
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
  
  start_dh(h1, [h1,h2]).

test2:-
  absolute_file_name(
    data('roman-emperors-copy'),
    File,
    [access(read),file_type(turtle)]
  ),
  rdf_load2(File, [format(turtle),graph('roman-emperors')]),
  rdf_create_dataset('roman-emperors', [], DS1),
  create_hive(h1, DS1, _H1),
  
  absolute_file_name(
    data('foaf'),
    File,
    [access(read),file_type(rdf)]
  ),
  rdf_load2(File, [graph(foaf)]),
  rdf_create_dataset(foaf, [], DS2),
  create_hive(h2, DS2, _H2),
  
  start_dh(h1, [h1,h2]).



:- begin_tests(dh).

% No unit tests yet...

:- end_tests(dh).

