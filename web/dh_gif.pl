:- module(
  dh_gif,
  [
    dh_graph/2 % -Gif:compound
               % +Options:list(nvpair)
  ]
).

/** <module> DataHives Graph Interchange Format

Produces descriptions of graphs in DataHives
using the Graph Interchange Format.

@author Wouter Beek
@version 2014/04-2014/07, 2014/11
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(list_ext)).
:- use_module(generics(typecheck)).
:- use_module(os(image_ext)).

:- use_module(plDcg(dcg_generics)).

:- use_module(plUri(image_uri)).
:- use_module(plUri(uri_ext)).

:- use_module(plHttp(download_to_file)).

:- use_module(plRdf(rdf_name)). % Meta-DCG.

:- use_module(dh(beh/com/dh_com)).
:- use_module(dh(beh/com/dh_edge_weight)).

:- predicate_options(dh_graph/2, 2, [
     pass_to(dh_graph/3, 3)
   ]).
:- predicate_options(dh_graph/3, 3, [
     pass_to(dh_graph/4, 4)
   ]).
:- predicate_options(dh_graph/4, 4, [
     pass_to(dh_graph/5, 5)
   ]).
:- predicate_options(dh_graph/5, 5, [
     pass_to(dh_graph_directed/3, 3),
     pass_to(dh_vertex_edge/4, 4),
     pass_to(dh_vertex_term/4, 4)
   ]).
:- predicate_options(dh_graph_directed/3, 3, [
     graph_directed(+boolean)
   ]).
:- predicate_options(dh_edge_term/5, 5, [
     pass_to(dh_edge_term_label/4, 4),
     pass_to(dh_edge_term_penwidth/5, 5)
   ]).
:- predicate_options(dh_edge_term_label/4, 4, [
     edge_label(+boolean)
   ]).
:- predicate_options(dh_edge_term_penwidth/5, 5, [
     edge_penwidth(+boolean)
   ]).
:- predicate_options(dh_vertex_term/4, 4, [
     pass_to(dh_vertex_term_image/4, 4),
     pass_to(dh_vertex_term_label/4, 4)
   ]).
:- predicate_options(dh_vertex_term_image/4, 4, [
     vertex_image(+boolean)
   ]).
:- predicate_options(dh_vertex_term_label/4, 4, [
     vertex_label(+boolean)
   ]).





% EDGES

%! dh_edge(-Triple:triple(or([bnode,iri,literal]))) is nondet.

dh_edge(S-P-O):-
  edge_count(rdf(S,P,O), _).



%! dh_edge_term(
%!   +MaxCount:nonneg,
%!   +Vertices:ordset,
%!   +Edge:compound,
%!   -EdgeTerm:compound,
%!   +Options
%! ) is det.

dh_edge_term(MaxCount, Vs, E, edge(FromId,ToId,Attrs3), Options):-
  E = From-_-To,
  nth0chk(FromId, Vs, From),
  nth0chk(ToId, Vs, To),
  Attrs1 = [],
  dh_edge_term_label(E, Attrs1, Attrs2, Options),
  dh_edge_term_penwidth(MaxCount, E, Attrs2, Attrs3, Options).



%! dh_edge_term_penwidth(
%!   +Edge:compound,
%!   +FromAttributes:list(nvpair),
%!   -ToAttributes:list(nvpair),
%!   +Options:list(nvpair)
%! ) is det.

dh_edge_term_label(_-P-_, Attrs, [label=Label|Attrs], Options):-
  option(edge_label(true), Options, true), !,
  dcg_with_output_to(atom(Label), rdf_term_name(P)).
dh_edge_term_label(_, Attrs, Attrs, _).



%! dh_edge_term_penwidth(
%!   +MaxCount:nonneg,
%!   +Edge:compound,
%!   +FromAttributes:list(nvpair),
%!   -ToAttributes:list(nvpair),
%!   +Options:list(nvpair)
%! ) is det.

dh_edge_term_penwidth(_, Attrs, Attrs, Options):-
  option(edge_penwidth(false), Options, true), !.
dh_edge_term_penwidth(0, _, Attrs, [penwidth=1|Attrs], _):- !.
dh_edge_term_penwidth(MaxCount, S-P-O, Attrs, [penwidth=Penwidth|Attrs], _):-
  edge_count(rdf(S,P,O), Count),
  Penwidth is Count / MaxCount * 10.





% GRAPHS

%! dh_graph(-Gif, +Options:list(nvpair)) is det.
% The following options are supported:
%   * =|edge(label(+DrawLabel:boolean))|=
%   * =|edge_penwidth(+DrawPenwidth:boolean)|=
%   * =|graph_directed(+IsDirected:boolean)|=
%   * =|vertex_image(+DrawImage:boolean)|=
%   * =|vertex_label(+DrawLabel:boolean)|=

dh_graph(Gif, Options):-
  % A special case occurs when there is no graph.
  (
    aggregate_all(
      max(Count),
      edge_count(_, Count),
      MaxCount
    )
  ->
    true
  ;
    MaxCount = 0
  ),
  dh_graph(MaxCount, Gif, Options).



%! dh_graph(
%!   +MaxCount:positive_integer,
%!   -Gif:compound,
%!   +Options:list(nvpair)
%! ) is det.

dh_graph(MaxCount, Gif, Options):-
  aggregate_all(
    set(E),
    dh_edge(E),
    Es
  ),
  dh_graph(MaxCount, Es, Gif, Options).



%! dh_graph(
%!   +MaxCount:positive_integer,
%!   +Edges:ordset(triple(or([bnode,iri,literal]))),
%!   -Gif:compound,
%!   +Options:list(nvpair)
%! ) is det.

dh_graph(MaxCount, Es, Gif, Options):-
  edges_to_vertices(Es, Vs),
  dh_graph(MaxCount, Vs, Es, Gif, Options).



%! dh_graph(
%!   +MaxCount:positive_integer,
%!   +Vertices:ordset(or([bnode,iri,literal])),
%!   +Edges:ordset(triple(or([bnode,iri,literal]))),
%!   -Gif:compound,
%!   +Options:list(nvpair)
%! ) is det.

dh_graph(MaxCount, Vs, Es, graph(VTerms,ETerms,Attrs2), Options):-
  findall(
    VTerm,
    (
      member(V, Vs),
      dh_vertex_term(Vs, V, VTerm, Options)
    ),
    VTerms
  ),
  findall(
    ETerm,
    (
      member(E, Es),
      dh_edge_term(MaxCount, Vs, E, ETerm, Options)
    ),
    ETerms
  ),
  Attrs1 = [],
  dh_graph_directed(Attrs1, Attrs2, Options).



%! dh_graph_directed(
%!   +FromAttributes:list(nvpair),
%!   -ToAttributes:list(nvpair),
%!   +Options:list(nvpair)
%! ) is det.

dh_graph_directed(Attrs, [directed=Directed|Attrs], Options):-
  option(graph_directed(Directed), Options, true), !.





% VERTICES

%! dh_vertex(-Vertex:or([bnode,iri,literal])) is nondet.

dh_vertex(V):-
  edge_count(rdf(V,_,_), _).
dh_vertex(V):-
  edge_count(rdf(_,_,V), _).



%! dh_vertex_term(
%!   +Vertices:ordset,
%!   +Vertex,
%!   -VertexTerm:compound,
%!   +Options:list(nvpair)
%! ) is det.

dh_vertex_term(Vs, V, vertex(Id,V,Attrs3), Options):-
  nth0chk(Id, Vs, V),
  Attrs1 = [],
  dh_vertex_term_image(V, Attrs1, Attrs2, Options),
  dh_vertex_term_label(V, Attrs2, Attrs3, Options).



%! dh_vertex_term_image(
%!   +Vertex,
%!   +FromAttributes:list(nvpair),
%!   -ToAttributes:list(nvpair),
%!   +Options:list(nvpair)
%! ) is det.

dh_vertex_term_image(V, Attrs, [image=CacheFile|Attrs], Options):-
  option(vertex_image(true), Options, true),
  is_image_uri(V), !,
  rdf_global_id(V, Url),
  uri_nested_file(data(.), Url, CacheFile),
  (   access_file(CacheFile, exist), !
  ;   download_to_file(Url, CacheFile, []), !
  ;   fail
  ).
dh_vertex_term_image(_, Attrs, Attrs, _).



%! dh_vertex_term_label(
%!   +Vertex,
%!   +FromAttributes:list(nvpair),
%!   -ToAttributes:list(nvpair),
%!   +Options:list(nvpair)
%! ) is det.

dh_vertex_term_label(V, Attrs, [label=Label|Attrs], Options):-
  option(vertex_label(true), Options, true), !,
  dcg_with_output_to(atom(Label), rdf_term_name([literal_ellipsis(50)], V)).
dh_vertex_term_label(_, Attrs, Attrs, _).





% HELPERS

edges_to_vertices([], []):- !.
edges_to_vertices([S-_-O|T], S3):-
  edges_to_vertices(T, S1),
  ord_add_element(S1, S, S2),
  ord_add_element(S2, O, S3).

