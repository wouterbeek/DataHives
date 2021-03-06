:- module(
  dh_random_walk,
  [
    dh_random_walk/2 % -DirectedTriple:compound
                     % +Options:list(nvpair)
  ]
).

/** <module> DataHives LOD

A very simple navigation strategy for DataHives
that uses the LOD stepping paradigm.

@author Wouter Beek
@version 2014/02-2014/07
*/

:- use_module(library(random)).

:- use_module(dh(beh/nav/dh_step)).
:- use_module(dh(beh/nav/dh_walk)).



%! dh_random_walk(-DirectedTriple:compound, +Options:list(nvpair)) is det.

dh_random_walk(DirTriple, Options):-
  dh_walk(dh_random_step, DirTriple, Options).

%! dh_random_step(
%!   +Resource:or([bnode,iri,literal]),
%!   -Triple:compound,
%!   +Options:list(nvpair)
%! ) is det.

dh_random_step(Resource, Triple, Options):-
  dh_step(random_member, Resource, Triple, Options).

