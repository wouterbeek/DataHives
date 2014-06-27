:- module(
  dh_bee_fly,
  [
    dh_bee_lod_fly/2 % +DirectedTriple:compound
                     % +Options:list(nvpair)
  ]
).

/** <module> DataHives bee navigation

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(dh(rdf_random_dbpedia)).
:- use_module(dh_core(dh_navigate)).
:- use_module(dh_nav(dh_step)).



%! dh_bee_lod_fly(+DirectedTriple:compound, +Options:list(nvpair)) is det.

dh_bee_lod_fly(DirTriple, Options):-
  dh_navigate(random_fly_step, DirTriple, Options).

%! random_fly_step(
%!   +Resource:or([bnode,iri,literal]),
%!   +Triple:compound,
%!   +Options:list(nvpair)
%! ) is det.

random_fly_step(Resource, Triple, Options):-
  dh_step(rdf_random_dbpedia_triple0, Resource, Triple, Options).

rdf_random_dbpedia_triple0(Triple, _):-
  rdf_random_dbpedia_triple(Triple).

