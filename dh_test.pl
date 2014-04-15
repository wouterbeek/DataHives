:- module(
  dh_test,
  [
    dh_test/1 % ?URL
  ]
).

/** <module> DataHives tests

Simple test predicates for running programs in DataHives.

@author Wouter Beek
@version 2013/09-2013/10, 2014/02
*/

:- use_module(dh(dh)).
:- use_module(dh(dh_walk)). % Meta-argument.
:- use_module(generics(meta_ext)).

default_url('http://dbpedia.org/resource/Banana').

dh_test(Url):-
  default_url(DefaultUrl),
  default(DefaultUrl, Url),
  init_agent(
    dh_random_walk,
    some_action,
    some_communication, %STUB
    Url
  ).

