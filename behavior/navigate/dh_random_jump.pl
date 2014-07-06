:- module(
  dh_random_jump,
  [
    dh_random_jump/2 % -DirectedTriple:compound
                     % +Options:list(nvpair)
  ]
).

/** <module> DataHives bee navigation

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06-2014/07
*/

:- use_module(library(option)).

:- use_module(plRdf(rdf_random)).

:- use_module(plSparql(sparql_random)).

:- use_module(dh_nav(dh_jump)).



%! dh_random_jump(-DirectedTriple:compound, +Options:list(nvpair)) is det.

dh_random_jump(DirectedTriple, Options):-
  dh_jump(dh_random_jump_triple, DirectedTriple, Options).

%! dh_random_jump(-Triple:compound, +Options:list(nvpair)) is det.
% The following options are supported:
%   * =|graph(+Graph:atom)|=
%     The name of the graph from which the random triples should be drawn.

% The randomly chosen triple must come from a locally loaded graph.
dh_random_jump_triple(Triple, Options):-
  option(graph(Graph), Options), !,
  rdf_random_triple(Triple, Graph).
% The randomly chosen triples must come from the LOD cloud.
% For convenience, we take a random triple from DBpedia instead.
dh_random_jump_triple(Triple, _):-
  sparql_random_triple(dbpedia, Triple).

