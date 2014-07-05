:- module(
  dh_random_lod_walk,
  [
    dh_random_lod_walk/2 % +DirectedTriple:compound
                         % +Options:list(nvpair)
  ]
).

/** <module> DataHives LOD

A very simple navigation strategy for DataHives
that uses the LOD stepping paradigm.

@author Wouter Beek
@version 2014/02-2014/06
*/

:- use_module(library(random)).

:- use_module(dh_nav(dh_navigate)).
:- use_module(dh_nav(dh_step)).



%! dh_random_lod_walk(+DirectedTriple:compound, +Options:list(nvpair)) is det.

dh_random_lod_walk(DirTriple, Options):-
  dh_navigate(dh_random_step, DirTriple, Options).

%! dh_random_step(+Resource, -Triple:compound, +Options:list(nvpair)) is det.

dh_random_step(Resource, Triple, Options):-
  dh_step(random_member, Resource, Triple, Options).

