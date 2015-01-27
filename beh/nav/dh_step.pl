:- module(
  dh_step,
  [
    dh_step/4 % :TripleSelection
              % +From:or([bnode,iri,literal])
              % -Triple:compound
              % +Options:list(nvpair)
  ]
).

/** <module> DataHives step

This module implements a stepping paradigm for possible use in DataHives.
More specifically, it specifies what it means to take a step in
the Linked Open Data cloud.

Navigation strategies that belong within this paradigm
are easily implemented by using dh_lod_step/3 from this module.

The reason to separate the implementation of a specific navigation strategy
from the characterization of a stepping paradigm is that:
  1. Multiple navigation strategies may have the same stepping paradigm.
  2. The same navigation strategy may apply to multiple stepping paradigms,
     e.g. random walks can be performed on the LOD cloud
     and on a locally loaded graph.

@author Wouter Beek
@version 2014/02-2014/06, 2014/11
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(db_ext)).

:- use_module(plRdf(rdf_gc)). % Run graph garbage collection.
:- use_module(plRdf(api/rdf_read)).

:- use_module(lodCache(lod_cache_egograph)).

:- db_add_novel(user:prolog_file_type(log, logging)).

:- predicate_options(dh_step/4, 4, [
     direction(+oneof([backward,both,forward])),
     graph(+atom),
     pass_to(load_cahce_egograph/3, 3)
   ]).

:- meta_predicate(dh_step(2,+,-,+)).
:- meta_predicate(dh_select_triple(2,+,-)).



%! dh_step(
%!   :TripleSelection,
%!   +From:or([bnode,iri,literal]),
%!   -Triple:compound,
%!   +Options:list(nvpair)
%! ) is det.
% Performs a step of the LOD cloud.
%
% First, we look up which propositions contain the given `Resource`.
% Lookup of propositions happend in various ways:
%   1. The propositions in which `Resource` appears are cached locally.
%   2. The propositions are retrieved via a SPARQL query.
%   3. The propositions are retrieved via dereferencing `Resource`.
% When step 2 or 3 is used, the results are cached locally,
% so that step 1 can be used in the future.
% When the local cache is full,
% cached results will be garbage collected,
% which implies that step 1 only works for a certain amount of time.
%
% Once we have all the propositions in which `Resource` appears,
% we select one of those according to the criterion implemented by goal
% `TripleSelection`.
%
% This means that the collection of propositions is always the same,
% i.e. everything we can find in the LOD cloud today,
% but the way in which we select a single proposition can differ.
%
% ### Options
%
% The following options are supported:
%   * =|direction(+Direction:oneof([backward,both,forward]))|=
%     The direction in which links are followed.
%     Default: `both`.
%   * =|endpoints(+Endpoints:list(atom))|=
%     A list of endpoint aliases.
%     Only these endpoints will be used to cache data from.
%     By default, all registered endpoints are considered.
%   * =|graph(+Graph:atom)|=
%     Use LOD that is locally stored in a graph.

% Do not use any endpoints, i.e. only use LOD that is stored locally.
dh_step(TripleSelection, Resource, Triple, Options):-
  option(graph(Graph), Options), !,
  option(direction(Direction), Options, both),
  rdf_direction_triples(Resource, Direction, Triples, Graph),
  dh_select_triple(TripleSelection, Triples, Triple).
% Remote LOD lookup: blank node.
dh_step(_, Resource, _, _):-
  rdf_is_bnode(Resource), !,
  fail.
% Remote LOD lookup: literal.
dh_step(_, Resource, _, _):-
  rdf_is_literal(Resource), !,
  fail.
% Remote LOD lookup: IRI.
dh_step(TripleSelection, Resource, Triple, Options):-
  % First we assert all triples that describe the given resource
  % (i.e., the depth-1 description or complete ego-graph).
  lod_cache_egograph(Resource, Triples, Options),

  % Then we pick one of those triples according to some method.
  dh_select_triple(TripleSelection, Triples, Triple).


%! dh_select_triple(
%!   :TripleSelection,
%!   +Triples:ordset(compound),
%!   -Triple:compound
%! ) is det.
% Selects a single triple with the given resource in the subject position.
%
% The strategy for selecting the triple is given by a binary predicate
% that must be deterministic.
%
% The resultant proposition is a Prolog list containing three RDF terms,
% forming an RDF triple.

dh_select_triple(TripleSelection, Triples, Triple):-
  % Your selection criterion is applied here.
  call(TripleSelection, Triple, Triples).

