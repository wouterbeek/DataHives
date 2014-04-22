:- module(
  dh_walk,
  [
    dh_random_walk/4 % +From:or([bnode,iri,literal])
                     % -Direction:oneof([backward,forward])
                     % -Link:iri
                     % -To:or([bnode,iri,literal])
  ]
).

/** <module> DataHives walking

@author Wouter Beek
@version 2014/02-2014/04
*/

:- use_module(library(aggregate)).
:- use_module(library(check_installation)). % Private predicate.
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- use_module(lod(lod_location)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(sparql(sparql_ext)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_gc_graph)). % Run graph garbage collection.
:- use_module(rdf_file(rdf_serial)).

:- meta_predicate(dh_walk(2,+,-,-,-)).
:- meta_predicate(lod_step(2,+,-)).
:- meta_predicate(select_triple(2,+,-)).



dh_random_walk(From, Dir, Link, To):-
  dh_walk(lod_random_step, From, Dir, Link, To).


%! dh_walk(
%!   :PossibleSteps,
%!   :ElectStep,
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

dh_walk(MakeStep, From, Dir, Link, To):-
  call(MakeStep, From, Proposition),

  % Instantiate the directionality parameter in order to indicate whether
  %  the walk is forward or backward directed.
  %
  % Notice that we prefer forward motion in case both directions
  %  are possible.
  % As a result of this it is impossible to walk *backwards* from
  %  =|rdfs:Class|= to itself, via =|rdf:type|=.
  (
    Proposition = [From,Link,To]
  ->
    Dir = forward
  ;
    Proposition = [To,Link,From]
  ->
    Dir = backward
  ).


%! lod_random_step(+Resource, -Proposition:list) is det.

lod_random_step(Resource, Proposition):-
  lod_step(random_member, Resource, Proposition).

%! lod_step(:Goal, +Resource, -Proposition:list) is det.

lod_step(Goal, Resource, Proposition):-
  % First we assert all triples that describe a resource (depth 1)
  % in a graph by that name.
  check_installation:run_collect_messages(
    dh_walk:assert_resource_graph(Resource),
    Status,
    Messages
  ),
  print_status(Status),
  maplist(print_message, Messages),
  % Then we pick on of those triples according to some method.
  select_triple(Goal, Resource, Proposition).
%lod_step(Goal, Resource, Proposition):-
%  gtrace, %DEB
%  lod_step(Goal, Resource, Proposition).

print_message(message(Term,Kind,_)):-
  print_message(Kind, Term).

print_status(false).
print_status(true).
print_status(exception(Error)):-
  print_message(error, Error).


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
% IRI: no in cache yet. Here we go...
assert_resource_graph(Resource):-
  % SPARQL query.
  assert_resource_graph_by_sparql_query(Resource),
  % IRI: download a LOD description based on the IRI prefix.
  assert_resource_graph_by_prefix(Resource),
  % IRI: based on the entire IRI we can download a LOD description,
  % i.e. a "dereference".
  assert_resource_graph_by_url(Resource).

assert_resource_graph_by_sparql_query(Resource):-
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
    Query
  ),
  sparql_query(Remote, Query, _, Rows),
  forall(
    member(row(P,O), Rows),
    rdf_assert(Resource, P, O, Resource)
  ).
assert_resource_graph_by_sparql_query(_).


assert_resource_graph_by_prefix(Resource):-
  rdf_global_id(Prefix:_, Resource),
  lod_location(Prefix, Url), !,
  assert_resource_graph_from_url(Resource, Url).
assert_resource_graph_by_prefix(_).


assert_resource_graph_by_url(Iri):-
  uri_iri(Uri, Iri),
  assert_resource_graph_from_url(Iri, Uri), !.
assert_resource_graph_by_url(_).


assert_resource_graph_from_url(Resource, Uri):-
  thread_self(Id),
  atomic_list_concat([Resource,Id], '_', ResourceThread),
  setup_call_catcher_cleanup(
    rdf_load_any([graph(ResourceThread)], Uri),
    rdf_copy(ResourceThread, Resource, _, _, Resource),
    Exception,
    (
      Exception = exception(Error)
    ->
      print_message(error, Error)
    ;
      rdf_unload_graph_debug(ResourceThread)
    )
  ).


%! select_triple(Goal, Resource, Proposition) is det.
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

