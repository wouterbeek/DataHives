:- module(
  dh_navigation,
  [
    dh_current_location/2, % +ThreadId:atom
                           % -CurrentLocation:or([bnode,iri,literal])
    dh_navigation_init/1, % +InitialLocation:or([bnode,iri,literal])
    dh_random_walk/4 % +From:or([bnode,iri,literal])
                     % -Direction:oneof([backward,forward])
                     % -Link:iri
                     % -To:or([bnode,iri,literal])
  ]
).

/** <module> DataHives navigation

Navigation predicates for agents in DataHives.

@author Wouter Beek
@version 2014/02-2014/04
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- use_module(lod(lod_location)).
:- use_module(pl(pl_log)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_gc_graph)). % Run graph garbage collection.
:- use_module(rdf_file(rdf_serial)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(sparql(sparql_ext)).

%! backtrack(
%!   ?From:or([bnode,iri,literal]),
%!   ?Direction:oneof([backward,forward]),
%!   ?Link:iri,
%!   ?To:or([bnode,iri,literal])
%! ) is det.

:- thread_local(backtrack/4).

:- meta_predicate(dh_navigate(2,+,-,-,-)).
:- meta_predicate(dh_step(2,+,-,-,-)).
:- meta_predicate(lod_step(2,+,-)).
:- meta_predicate(select_triple(2,+,-)).



%! dh_current_location(
%!   +ThreadId:atom,
%!   -CurrentLocation:or([bnode,iri,literal])
%! ) is det.

dh_current_location(ThreadId, CurrentLocation):-
  thread_signal(ThreadId, backtrack(_,_,_,CurrentLocation)).



% NAVIGATION STRATEGIES %

%! dh_random_walk(
%!   +From:or([bnode,iri,literal]),
%!   -Direction:oneof([backward,forward]),
%!   -Link:iri,
%!   -To:or([bnode,iri,literal])
%! ) is det.

dh_random_walk(From, Dir, Link, To):-
  dh_navigate(lod_random_step, From, Dir, Link, To).


%! lod_random_step(+Resource, -Proposition:list) is det.

lod_random_step(Resource, Proposition):-
  lod_step(random_member, Resource, Proposition).



% GENERIC NAVIGATION PREDICATE %

dh_navigation_init(InitFrom):-
  assert(
    backtrack(
      InitFrom,
      forward,
      'http://www.w3.org/2002/07/owl#sameAs',
      InitFrom
    )
  ).


%! dh_navigate(
%!   :Navigation,
%!   +From:or([bnode,iri,literal]),
%!   -Direction:oneof([backward,forward]),
%!   -Link:iri,
%!   -To:or([bnode,iri,literal])
%! ) is det.
% Walk from a single location (called `From`) to a single location
%  (called `To`) via `Link`.
%
% Walking in DataHives is bidirectional.
%
% We must return a specific step.
% We used to return all possible steps and make a selection based on those.
% The latter was inefficient in the case of very connected nodes.

dh_navigate(Nav, From, Dir, Link, To):-
  backtrack(_, _, _, From),
  (
    dh_step(Nav, From, Dir, Link, To)
  ->
    retract(backtrack(_, _, _, _)),
    assert(backtrack(From, Dir, Link, To))
  ;
    retract(backtrack(To, Dir0, Link, From)),
    dir_inv(Dir0, Dir),
    assert(backtrack(From, Dir, Link, To))
  ).

dir_inv(backward, forward).
dir_inv(forward, backward).


%! dh_step(
%!   :Navigation,
%!   +From:or([bnode,iri,literal]),
%!   -Direction:oneof([backward,forward]),
%!   -Link:iri,
%!   -To:or([bnode,iri,literal])
%! ) is det.

dh_step(Nav, From, Direction, Link, To):-
  call(Nav, From, Proposition),

  % Instantiate the directionality parameter in order to indicate whether
  % the walk is forward or backward directed.
  %
  % Notice that we prefer forward motion in case both directions
  % are possible.
  % As a result of this it is impossible to walk *backwards* from
  % =|rdfs:Class|= to itself, via =|rdf:type|=.
  (
    Proposition = [From,Link,To]
  ->
    Direction = forward
  ;
    Proposition = [To,Link,From]
  ->
    Direction = backward
  ).


%! lod_step(
%!   :Goal,
%!   +Resource:or([bnode,iri,literal]),
%!   -Proposition:list(or([bnode,iri,literal]))
%! ) is det.

lod_step(Goal, Resource, Proposition):-
  absolute_file_name(data(dh), File, [access(write),file_type(logging)]),
  % First we assert all triples that describe a resource (depth 1)
  % in a graph by that name.
  run_collect_messages(
    assert_resource_graph(Resource),
    File
  ),
  % Then we pick on of those triples according to some method.
  select_triple(Goal, Resource, Proposition).


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
  % SPARQL query.
  ignore(catch(assert_resource_graph_by_sparql_query(Resource, Cached1), _, true)),
  % IRI: download a LOD description based on the IRI prefix.
  ignore(catch(assert_resource_graph_by_prefix(Resource, Cached2), _, true)),
  % IRI: based on the entire IRI we can download a LOD description,
  % i.e. a "dereference".
  ignore(catch(assert_resource_graph_by_url(Resource, Cached3), _, true)),
  % DEB
  report_on_caching(Cached1, Cached2, Cached3).

report_on_caching(C1, C2, C3):-
  format(user_output, 'CACHING: ', []),
  (C1 == true -> format(user_output, ' SPARQL', []) ; true),
  (C2 == true -> format(user_output, ' PREFIX', []) ; true),
  (C3 == true -> format(user_output, ' DEREF',  []) ; true),
  format(user_output, '~n', []).


%! assert_resource_graph_by_sparql_query(
%!   +Resource:iri,
%!   -Cached:boolean
%! ) is det.

assert_resource_graph_by_sparql_query(Resource, true):-
  uri_components(Resource, uri_components(_, Domain, _, _, _)),
  sparql_current_remote_domain(Remote, Domain), !,
  
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
  
  (Rows1 == [], Rows2 == [] -> gtrace ; true), %DEB
  
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


%! select_triple(
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

select_triple(Goal, Resource, Proposition):-
  % Ensure uniqueness of propositions by gathering them in an ordered set.
  aggregate_all(
    set([Resource,P,O]),
    rdf(Resource, P, O, Resource),
    Propositions
  ),
  % Your selection criterion is applied here.
  call(Goal, Proposition, Propositions).

