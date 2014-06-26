:- module(
  dh_random_lod_walk,
  [
    dh_random_lod_walk/4, % +From:or([bnode,iri,literal]),
                          % -Direction:oneof([backward,forward]),
                          % -Link:iri,
                          % -To:or([bnode,iri,literal])
    dh_random_lod_walk/5 % +Graph:atom
                         % +From:or([bnode,iri,literal]),
                         % -Direction:oneof([backward,forward]),
                         % -Link:iri,
                         % -To:or([bnode,iri,literal])
  ]
).

/** <module> DataHives LOD

A very simple navigation strategy for DataHives
that uses the LOD stepping paradigm.

@author Wouter Beek
@version 2014/02-2014/06
*/

:- use_module(library(random)).
:- use_module(library(semweb/rdf_db)). % Declarations.

:- use_module(dh_core(dh_navigate)).
:- use_module(dh_nav(dh_step)).

:- rdf_meta(dh_random_lod_walk(o,-,r,o)).
:- rdf_meta(dh_random_lod_walk(+,o,-,r,o)).
:- rdf_meta(dh_random_step(r,-)).



%! dh_random_lod_walk(
%!   +From:or([bnode,iri,literal]),
%!   -Direction:oneof([backward,forward]),
%!   -Link:iri,
%!   -To:or([bnode,iri,literal])
%! ) is det.

dh_random_lod_walk(From, Dir, Link, To):-
  dh_navigate(dh_random_step, From, Dir, Link, To).

%! dh_random_lod_walk(
%!   +Graph:atom,
%!   +From:or([bnode,iri,literal]),
%!   -Direction:oneof([backward,forward]),
%!   -Link:iri,
%!   -To:or([bnode,iri,literal])
%! ) is det.

dh_random_lod_walk(Graph, From, Dir, Link, To):-
  dh_navigate(dh_random_step(Graph), From, Dir, Link, To).


%! dh_random_step(+Resource, -Triple:compound) is det.

dh_random_step(Resource, Triple):-
  dh_step(random_member, Resource, Triple, []).

%! dh_random_step(+Graph:atom, +Resource, -Triple:compound) is det.

dh_random_step(Graph, Resource, Triple):-
  dh_step(Graph, random_member, Resource, Triple, []).

