:- module(
  dh_type_check,
  [
    dh_type_check/1 % ?URL
  ]
).

/** <module> DataHives: Type check

Verify the well-formedness of IRIs.

@author Wouter Beek
@version 2014/02
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- use_module(generic(meta_ext)).

:- use_module(dh(dh)).
:- use_module(dh(dh_walk)). % Meta-argument.

default_url('http://dbpedia.org/resource/Banana').



dh_type_check(Url):-
  default_url(DefaultUrl),
  default(DefaultUrl, Url),
  init_agent(
    dh_random_walk,
    type_check,
    default_communication, %STUB
    Url
  ).

type_check(_, _, _, To):-
  type_check(To).

:- dynamic(malformed_uri/2).
type_check(To):-
  rdf_is_bnode(To), !.
type_check(To):-
  rdf_is_literal(To), !.
type_check(To):-
  uri_components(To, _), !.
type_check(To):-
  thread_self(Alias),
  with_mutex(dh_type_check, malformed_uri(Alias, To)), !.
type_check(To):-
  thread_self(Alias),
  with_mutex(dh_type_check, assert(malformed_uri(Alias, To))).

