:- module(
  dh_test,
  [
    dh_test/1 % ?URL
  ]
).

/** <module> DataHives tests

Simple test predicates for running programs in DataHives.

@author Wouter Beek
@version 2013/09-2013/10, 2014/02, 2014/04
*/

:- use_module(library(aggregate)).
:- use_module(library(random)).

:- use_module(dh(dh)).
:- use_module(dh(dh_walk)). % Meta-argument.
:- use_module(generics(meta_ext)).

:- dynamic(start_url/2).

%start_url('http://dbpedia.org/resource/Banana').
start_url('http://rdf.freebase.com/ns/m.08pbxl').



dh_test(Url):-
  aggregate_all(
    set(StartUrl),
    start_url(StartUrl),
    StartUrls
  ),
  random_member(Url, StartUrls),
  init_agent(
    dh_random_walk,
    some_action,
    some_communication, %STUB
    Url
  ).

