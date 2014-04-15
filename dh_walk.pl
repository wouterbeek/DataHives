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

:- use_module(generics(list_ext)). % Meta-argument.
:- use_module(generics(meta_ext)).
:- use_module(generics(uri_ext)).
:- use_module(http(http_download)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module(lod(lod_location)).
:- use_module(os(file_mime)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(sparql(sparql_ext)).
:- use_module(rdf(rdf_gc_graph)). % Run graph garbage collection.
:- use_module(rdf(rdf_meta)).
:- use_module(rdf_file(rdf_file)).
:- use_module(rdf_file(rdf_serial)).



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

:- meta_predicate(dh_walk(2,+,-,-,-)).
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

% 0. Blank nodes and literals always fail.
lod_random_step(Resource, _):-
  rdf_is_bnode(Resource), !,
  fail.
lod_random_step(Resource, _):-
  rdf_is_literal(Resource), !,
  fail.

% 1. Query a registered SPARQL endpoint.
lod_random_step(Resource, Proposition):-
  sparql_random_step(Resource, Proposition).

% 2. Download a LOD description based on the IRI prefix.
lod_random_step(Resource, Proposition):-
  rdf_global_id(Prefix:_, Resource),
  (
    lod_location(Prefix, URL), !
  ;
    URL = Resource
  ),
  lod_local_query([], URL, Prefix, Resource, Proposition).

% 3. Based on the entire IRI we can download a LOD description.
lod_random_step(Resource, Proposition):-
  is_of_type(uri, Resource), !,
  atomic_list_concat([graph,Resource], '_', Graph),
  lod_local_query([], Resource, Graph, Resource, Proposition).


%! lod_local_query(
%!   +Options:list(nvpair),
%!   +URL:url,
%!   +Graph:atom,
%!   +Resource:or([bnode,iri,literal]),
%!   -Proposition:list(or([bnode,iri,literal]))
%! ) is det.
% The options are passed to download_to_file/3 -> http_goal -> http_open/3.

lod_local_query(_, _, Graph, Resource, Proposition):-
  rdf_graph(Graph), !,
  lod_local_query_on_loaded_graph(Graph, Resource, Proposition).
lod_local_query(O1, URL, Graph, Resource, Proposition):-
  setup_call_cleanup(
    catch(download_to_file(O1, URL, File), _, fail),
    lod_local_query_on_file(File, Graph, Resource, Proposition),
    % Keep the graph loaded, but delete the file.
    catch(delete_file(File), _, true)
  ).


% Potential RDF! Let's try to load it in a graph.
lod_local_query_on_file(File, Graph, Resource, Proposition):-
  file_mime(File, MIME),
  rdf_mime(MIME), !,
  rdf_load([mime(MIME)], Graph, File),
  lod_local_query_on_loaded_graph(Graph, Resource, Proposition).
% There is no joy in this: no RDF.
lod_local_query_on_file(File, _, _, _):-
  debug(dh_walk, 'No RDF in file ~w.', [File]),
  fail.


lod_local_query_on_loaded_graph(Graph, Resource, Proposition):-
  aggregate_all(
    set([Resource,P,O]),
    rdf(Resource, P, O),
    Propositions1
  ),
  aggregate_all(
    set([S,P,Resource]),
    rdf(S, P, Resource),
    Propositions2
  ),
  ord_union(Propositions1, Propositions2, Propositions),
  random_member(Proposition, Propositions),
  
  Proposition = [S,P,O],
  forall(
    rdf(S, P, O, Graph),
    rdf_graph_touch(Graph)
  ).


sparql_random_step(Resource, Proposition):-
  uri_components(Resource, uri_components(_, Domain, _, _, _)),
  sparql_current_remote_domain(Remote, Domain), !,
  
  atomic_list_concat([graph,Resource], '_', Graph),
  sparql_random_step(Remote, Graph, Resource, Proposition).

sparql_random_step(_, Graph, Resource, Proposition):-
  rdf_graph(Graph), !,
  lod_local_query_on_loaded_graph(Graph, Resource, Proposition).
sparql_random_step(Remote, Graph, Resource, Proposition):-
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
  maplist(assert_proposition_prefix(Graph, Resource), Rows1),

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
  maplist(assert_proposition_postfix(Graph, Resource), Rows2),

  sparql_random_step(Remote, Graph, Resource, Proposition).

assert_proposition_prefix(G, S, row(P,O)):-
  rdf_assert(S, P, O, G).
assert_proposition_postfix(G, O, row(S,P)):-
  rdf_assert(S, P, O, G).


/*
%! sparql_random_step(
%!   +Resource:iri,
%!   -Proposition:list(or([bnode,iri,literal]))
%! ) is det.
% Notice that resources that appear in both the subject and object position
%  of a triple -- see example [1] -- have a higher chance of being chosen.
%
% ~~~{.rdf}
% <rdfs:Class, rdf:type, rdfs:Class>
% ~~~

sparql_random_step(Resource, Proposition):-
  uri_components(Resource, uri_components(_, Domain, _, _, _)),
  sparql_current_remote_domain(Remote, Domain), !,

  % Direction: forward.
  phrase(
    sparql_count(_, _, [], o, [rdf(iri(Resource),var(p),var(o))]),
    Query1
  ),
  sparql_query(Remote, Query1, _, [row(literal(type(D1,Lit1)))]),
  rdf_lexical_map(Lit1, D1, _, Count1),

  % Direction: backward.
  phrase(
    sparql_count(_, _, [], s, [rdf(var(s),var(p),iri(Resource))]),
    Query2
  ),
  sparql_query(Remote, Query2, _, [row(literal(type(D2,Lit2)))]),
  rdf_lexical_map(Lit2, D2, _, Count2),

  Count is Count1 + Count2,
  random_between(1, Count, N),

  (
    N =< Count1
  ->
    phrase(
      sparql_formulate(
        _,
        _,
        [],
        select,
        true,
        [p,o],
        [rdf(iri(Resource),var(p),var(o))],
        1,
        N,
        _
      ),
      Query3
    ),
    sparql_query(Remote, Query3, _, [row(P,O)]),
    Proposition = [Resource,P,O]
  ;
    M is N - Count1,
    phrase(
      sparql_formulate(
        _,
        _,
        [],
        select,
        true,
        [s,p],
        [rdf(var(s),var(p),iri(Resource))],
        1,
        M,
        _
      ),
      Query3
    ),
    sparql_query(Remote, Query3, _, [row(S,P)]),
    Proposition = [S,P,Resource]
  ).
*/

