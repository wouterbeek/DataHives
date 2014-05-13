:- module(
  dh_init,
  [
    start_url/1 % ?Url:url
  ]
).

/** <module> DataHives Init
Some URLs we use for initializing test cases.
*/

:- dynamic(start_url/1).

start_url('http://dbpedia.org/resource/Banana').
%start_url('http://rdf.freebase.com/ns/m.08pbxl').

