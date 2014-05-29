:- module(
  lod_step,
  [
    lod_step/3 % :PropositionSelection
               % +From:or([bnode,iri,literal])
               % -Proposition:list(or([bnode,iri,literal]))
  ]
).

/** <module> Linked Open Data step

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
@version 2014/02-2014/05
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- use_module(generics(db_ext)).
:- use_module(lod(lod_location)).
:- use_module(pl(pl_log)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(sparql(sparql_ext)).

:- use_module(plRdf(rdf_build)).
:- use_module(plRdf(rdf_gc_graph)). % Run graph garbage collection.
:- use_module(plRdf_ser(rdf_serial)).

:- dynamic(no_dereference/1).

:- db_add_novel(user:prolog_file_type(log, logging)).

:- meta_predicate(lod_step(2,+,-)).
:- meta_predicate(lod_select_triple(2,+,-)).



%! lod_step(
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
% i.e. everything we can find in the LOD could today,
% but the way in which we select a single proposition can differ.

lod_step(Goal, Resource, Proposition):-
  absolute_file_name(data(dh), File, [access(write),file_type(logging)]),
  % First we assert all triples that describe a resource (depth 1)
  % in a graph by that name.
  run_collect_messages(
    assert_resource_graph(Resource),
    File
  ),
  % Then we pick one of those triples according to some method.
  lod_select_triple(Goal, Resource, Proposition).


% RDF blank node.
assert_resource_graph(Resource):-
  rdf_is_bnode(Resource), !.
% RDF literal.
assert_resource_graph(Resource):-
  rdf_is_literal(Resource), !.
% IRI: already there, touch it to ensure it remains in cache a bit longer.
assert_resource_graph(Resource):-
  rdf_graph(Resource), !,
  rdf_graph_touch(Resource).
% IRI: not in cache yet. Here we go...
assert_resource_graph(Resource):-
  no_dereference(Resource), !.
assert_resource_graph(Resource):-
  % SPARQL query.
  ignore(catch(assert_resource_graph_by_sparql_query(Resource, Cached1), _, true)),

  % IRI: download a LOD description based on the IRI prefix.
  ignore(catch(assert_resource_graph_by_prefix(Resource, Cached2), _, true)),

  % IRI: based on the entire IRI we can download a LOD description,
  % i.e. a "dereference".
  ignore(catch(assert_resource_graph_by_url(Resource, Cached3), _, true)),

  % DEB
  report_on_caching(Resource, Cached1, Cached2, Cached3),

  % Can I dereference this resource?
  register_dereferenceability(Cached1, Cached2, Cached3, Resource).

report_on_caching(Resource, Cached1, Cached2, Cached3):-
  format(user_output, 'CACHING ~w: ', [Resource]),
  (Cached1 == true -> format(user_output, ' SPARQL', []) ; true),
  (Cached2 == true -> format(user_output, ' PREFIX', []) ; true),
  (Cached3 == true -> format(user_output, ' DEREF',  []) ; true),
  format(user_output, '~n', []).

register_dereferenceability(false, false, false, Resource):- !,
  assert(no_dereference(Resource)).
register_dereferenceability(_, _, _, _).


%! assert_resource_graph_by_sparql_query(
%!   +Resource:iri,
%!   -Cached:boolean
%! ) is det.

assert_resource_graph_by_sparql_query(Resource, true):-
  uri_components(Resource, uri_components(_, Domain1, _, _, _)),
  (
    Domain1 == 'dbpedia.org'
  ->
    Domain2 = 'live.dbpedia.org'
  ;
    true
  ),
  sparql_current_remote_domain(Remote, Domain2), !,

  % Find predicate-object pairs.
  phrase(
    sparql_formulate(
      _,
      _,
      [],
      select,
      true,
      [p,o],
      [rdf(iri(Resource),var(p),var(o))],
      inf,
      _,
      _
    ),
    Query1
  ),
  sparql_query(Remote, Query1, _, Rows1),

  % Find subject-predicate pairs.
  phrase(
    sparql_formulate(
      _,
      _,
      [],
      select,
      true,
      [s,p],
      [rdf(var(s),var(p),iri(Resource))],
      inf,
      _,
      _
    ),
    Query2
  ),
  sparql_query(Remote, Query2, _, Rows2),

  (Rows1 == [], Rows2 == [] -> true ; true), %DEB

  forall(
    (
      member(row(P,O), Rows1)
    ;
      member(row(P,O), Rows2)
    ),
    rdf_assert(Resource, P, O, Resource)
  ).
assert_resource_graph_by_sparql_query(_, false).


%! assert_resource_graph_by_prefix(+Resource:iri, -Cached:boolean) is det.

assert_resource_graph_by_prefix(Resource, true):-
  rdf_global_id(Prefix:_, Resource),
  lod_location(Prefix, Url), !,
  assert_resource_graph_by_url_1(Resource, Url).
assert_resource_graph_by_prefix(_, false).


%! assert_resource_graph_by_url(+Iri:iri, -Cached:boolean) is det.

assert_resource_graph_by_url(Iri, true):-
  uri_iri(Url, Iri),
  assert_resource_graph_by_url_1(Iri, Url), !.
assert_resource_graph_by_url(_, false).

%! assert_resource_graph_by_url_1(+Resource:iri, +Url:url) is det.

assert_resource_graph_by_url_1(Resource, Url):-
  thread_self(Id),
  atomic_list_concat([Resource,Id], '_', ResourceThread),
  setup_call_catcher_cleanup(
    rdf_load_any([], Url, [_-ResourceThread]),
    rdf_copy(ResourceThread, Resource, _, _, Resource),
    Exception,
    (
      Exception = exception(Error)
    ->
      print_message(error, Error)
    ;
      rdf_unload_graph_debug(ResourceThread)
    )
  ),
  (rdf_graph(Url) -> true ; gtrace). %DEB


%! lod_select_triple(
%!   :Goal,
%!   +Resource:or([bnode,iri,literal]),
%!   -Proposition:list(or([bnode,iri,literal]))
%! ) is det.
% Selects a single triple with the given resource in the subject position.
%
% The strategy for selecting the triple is given by a binary predicate
% that must be deterministic.
%
% The resultant proposition is a Prolog list containing three RDF terms,
% forming an RDF triple.

lod_select_triple(Goal, Resource, Proposition):-
  % Ensure uniqueness of propositions by gathering them in an ordered set.
  aggregate_all(
    set([Resource,P,O]),
    rdf(Resource, P, O, Resource),
    Propositions
  ),
  % Your selection criterion is applied here.
  call(Goal, Proposition, Propositions).

