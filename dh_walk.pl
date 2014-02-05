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
@version 2014/02
*/

:- use_module(generics(list_ext)). % Meta-argument.
:- use_module(generics(meta_ext)).
:- use_module(generics(uri_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module('LOD'('LOD_location')).
:- use_module(os(file_mime)).
:- use_module('SPARQL'('SPARQL_build')).
:- use_module('SPARQL'('SPARQL_db')).
:- use_module('SPARQL'('SPARQL_ext')).
:- use_module(rdf(rdf_meta)).
:- use_module(rdf(rdf_serial)).



dh_random_walk(From, Dir, Link, To):-
  dh_walk('LOD_step', random_member, From, Dir, Link, To).


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
% The possible steps are enumerated by `PossibleSteps`.
% The actual step that is returned is then elected by `ElectStep`.

:- meta_predicate(dh_walk(2,2,+,-,-,-)).
dh_walk(PossibleSteps, ElectStep, From, Dir, Link, To):-
  call(PossibleSteps, From, Propositions),
  call(ElectStep, Proposition, Propositions),

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


% 1. Query a registered SPARQL endpoint.
'LOD_step'(Resource, Propositions):-
  'SPARQL_step'(Resource, Propositions).

% 2. Download a LOD description based on the IRI prefix.
'LOD_step'(Resource, Propositions):-
  rdf_global_id(Prefix:_, Resource),
  (
    'LOD_location'(Prefix, URL), !
  ;
    URL = Resource
  ),
  'LOD_local_query'([], URL, Prefix, Resource, Propositions).

% 3. Based on the entire IRI we can download a LOD description.
'LOD_step'(Resource, Propositions):-
  is_of_type(uri, Resource), !,
  'LOD_local_query'([], Resource, _NoGraph, Resource, Propositions).


%! 'LOD_local_query'(
%!   +Options:list(nvpair),
%!   +URL:url,
%!   +Graph:atom,
%!   +Resource:or([bnode,iri,literal]),
%!   -Propositions:ordset(list(or([bnode,iri,literal])))
%! ) is det.
% The options are passed to download_to_file/3 -> http_goal -> http_open/3.

'LOD_local_query'(_, _, Graph, Resource, Propositions):-
  rdf_graph(Graph), !,
  'LOD_local_query_on_loaded_graph'(Resource, Propositions, Graph).
'LOD_local_query'(O1, URL, Graph, Resource, Propositions):-
  catch(download_to_file(O1, URL, File), _, fail),
  'LOD_local_query_on_file'(File, Graph, Resource, Propositions).


% Potential RDF! Let's try to load it in a graph.
'LOD_local_query_on_file'(File, Graph, Resource, Propositions):-
  file_mime(File, MIME),
  rdf_mime(MIME), !,
  'LOD_local_query_on_graph'(File, MIME, Graph, Resource, Propositions).
% There is no joy in this: no RDF.
'LOD_local_query_on_file'(File, _, _, [], []):-
  debug(cache_it, 'No RDF in file ~w.', [File]),
  delete_file(File).


% The graph first needs to be loaded.
'LOD_local_query_on_graph'(File, MIME, Graph, Resource, Propositions):-
  % If graph is nonvar, it is kept.
  % If graph is var, it is erased.
  rdf_setup_call_cleanup(
    [graph(Graph),mime(MIME)],
    File,
    'LOD_local_query_on_loaded_graph'(Resource, Propositions)
  ).


'LOD_local_query_on_loaded_graph'(Resource, Propositions, Graph):-
  setoff(
    [Resource,P,O],
    rdf(Resource, P, O, Graph),
    Propositions1
  ),
  setoff(
    [S,P,Resource],
    rdf(S, P, Resource, Graph),
    Propositions2
  ),
  ord_union(Propositions1, Propositions2, Propositions).


'SPARQL_step'(Resource, Propositions):-
  uri_components(Resource, uri_components(_, Domain, _, _, _)),
  'SPARQL_current_remote_domain'(Remote, Domain), !,

  % Direction: forward.
  phrase(
    'SPARQL_formulate'(
      _,
      _,
      [],
      select,
      true,
      [p,o],
      [rdf(iri(Resource),var(p),var(o))],
      inf,
      _
    ),
    Query1
  ),
  'SPARQL_query'(Remote, Query1, _VarNames1, Rows1),
  maplist(prefixed_row(Resource), Rows1, Propositions1),

  % Direction: backward.
  phrase(
    'SPARQL_formulate'(
      _,
      _,
      [],
      select,
      true,
      [s,p],
      [rdf(var(s),var(p),iri(Resource))],
      inf,
      _
    ),
    Query2
  ),
  'SPARQL_query'(Remote, Query2, _VarNames2, Rows2),
  maplist(postfixed_row(Resource), Rows2, Propositions2),

  ord_union(Propositions1, Propositions2, Propositions).

prefixed_row(S, row(P,O), [S,P,O]).
postfixed_row(O, row(S,P), [S,P,O]).

