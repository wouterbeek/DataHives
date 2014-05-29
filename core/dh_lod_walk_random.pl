:- module(
  dh_lod_walk_random,
  [
    dh_lod_walk_random/4 % +From:or([bnode,iri,literal]),
                         % -Direction:oneof([backward,forward]),
                         % -Link:iri,
                         % -To:or([bnode,iri,literal])
  ]
).

/** <module> DataHives LOD

A very simple navigation strategy for DataHives
that uses the LOD stepping paradigm.

@author Wouter Beek
@version 2014/02-2014/05
*/

:- use_module(library(random)).

:- use_module(dh_core(dh_navigation)).
:- use_module(dh_core(lod_step)).



%! dh_lod_walk_random(
%!   +From:or([bnode,iri,literal]),
%!   -Direction:oneof([backward,forward]),
%!   -Link:iri,
%!   -To:or([bnode,iri,literal])
%! ) is det.

dh_lod_walk_random(From, Dir, Link, To):-
  dh_navigate(lod_random_step, From, Dir, Link, To).


%! lod_random_step(+Resource, -Proposition:list) is det.

lod_random_step(Resource, Proposition):-
  lod_step(random_member, Resource, Proposition).

