:- module(
  dh_test_generics,
  [
    random_start_url/1, % -Url:url
    start_url/1 % ?Url:url
  ]
).

/** <module> DataHives test generics

Generic support for DataHives test modules.

@author Wouter Beek
@author Baudouin Duthoit
@version 2014/05-2014/06
*/

:- use_module(library(aggregate)).
:- use_module(library(random)).

:- dynamic(start_url/1).



%! random_start_url(-Url:url) is det.
% Returns a randomly chosen URL from the start_url/1 pool.

random_start_url(Url):-
  aggregate_all(
    set(Url),
    start_url(Url),
    Urls
  ),
  random_member(Url, Urls).


% DBpedia: ontology
start_url('http://dbpedia.org/ontology/timeZone').
% DBpedia: resource
start_url('http://dbpedia.org/resource/Banana').
% Freebase
start_url('http://rdf.freebase.com/ns/m.08pbxl').
