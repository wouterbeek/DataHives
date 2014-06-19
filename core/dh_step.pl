:- module(
  dh_step,
  [
    dh_step/3 % :PropositionSelection
              % +From:or([bnode,iri,literal])
              % -Proposition:list(or([bnode,iri,literal]))
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
@version 2014/02-2014/06
*/

:- use_module(generics(db_ext)).

:- use_module(plRdf(rdf_gc)). % Run graph garbage collection.

:- use_module(lodCache(lod_cache_egograph)).

:- db_add_novel(user:prolog_file_type(log, logging)).

:- meta_predicate(dh_step(2,+,-)).
:- meta_predicate(dh_select_triple(2,+,-)).



%! dh_step(
%!   :PropositionSelection,
%!   +From:or([bnode,iri,literal]),
%!   -Proposition:list(or([bnode,iri,literal]))
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
% `PropositionSelection`.
%
% This means that the collection of propositions is always the same,
% i.e. everything we can find in the LOD cloud today,
% but the way in which we select a single proposition can differ.

dh_step(Goal, Resource, Proposition):-
  % First we assert all triples that describe the given resource
  % (i.e., the depth-1 description or copmplete ego-graph).
  assert_egograph(Resource, Propositions),

  % Then we pick one of those triples according to some method.
  dh_select_triple(Goal, Propositions, Proposition).


%! dh_select_triple(
%!   :Goal,
%!   +Propositions:ordset(list(or([bnode,iri,literal]))),
%!   -Proposition:list(or([bnode,iri,literal]))
%! ) is det.
% Selects a single triple with the given resource in the subject position.
%
% The strategy for selecting the triple is given by a binary predicate
% that must be deterministic.
%
% The resultant proposition is a Prolog list containing three RDF terms,
% forming an RDF triple.

dh_select_triple(Goal, Propositions, Proposition):-
  % Your selection criterion is applied here.
  call(Goal, Proposition, Propositions).

