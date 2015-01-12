:- module(
  dh_export_graph,
  [
    dh_export_graph/2 % -ExportGraph:compound
                      % +Options:list(nvpair)
  ]
).

/** <module> DataHives Graph Interchange Format

Produces descriptions of graphs in DataHives
using the Graph Interchange Format.

@author Wouter Beek
@version 2014/04-2014/07, 2014/11-2015/01
*/

:- use_module(library(aggregate)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plDcg(dcg_generics)).

:- use_module(plUri(image_uri)).
:- use_module(plUri(uri_ext)).

:- use_module(plGraph(l_graph/l_graph)).

:- use_module(plHttp(download_to_file)).

:- use_module(plGraphDraw(build_export_graph)).

:- use_module(plRdf(rdf_name)). % Meta-DCG.

:- use_module(dh(beh/com/dh_com)).
:- use_module(dh(beh/com/dh_edge_weight)).

:- predicate_options(dh_export_graph/2, 2, [
     pass_to(dh_export_graph/3, 3)
   ]).
:- predicate_options(dh_export_graph/3, 3, [
     pass_to(dh_export_graph/4, 4)
   ]).
:- predicate_options(dh_export_graph/4, 4, [
     pass_to(build_export_graph/4, 4)
   ]).





%! dh_edge_label(+Edge:compound, -Label:atom) is det.

dh_edge_label(edge(_,P,_), Label):-
  dcg_with_output_to(atom(Label), rdf_term_name(P)).



%! dh_edge_penwidth(
%!   +MaxCount:nonneg,
%!   +Edge:compound,
%!   -Penwidth:nonneg
%! ) is det.

dh_edge_penwidth(0, _, 1):- !.
dh_edge_penwidth(MaxCount, edge(S,P,O), Penwidth):-
  edge_count(rdf(S,P,O), Count),
  Penwidth is Count / MaxCount * 10.



%! dh_export_graph(-ExportGraph:compound, +Options:list(nvpair)) is det.

dh_export_graph(ExportGraph, Options):-
  % A special case occurs when there is no graph.
  (   aggregate_all(
        max(Count),
        edge_count(_, Count),
        MaxCount
      )
  ->  true
  ;   MaxCount = 0
  ),
  dh_export_graph(MaxCount, ExportGraph, Options).



%! dh_export_graph(
%!   +MaxCount:positive_integer,
%!   -ExportGraph:compound,
%!   +Options:list(nvpair)
%! ) is det.

dh_export_graph(MaxCount, ExportGraph, Options):-
  aggregate_all(
    set(edge(S,P,O)),
    edge_count(rdf(S,P,O), _),
    Es
  ),
  dh_export_graph(MaxCount, Es, ExportGraph, Options).



%! dh_export_graph(
%!   +MaxCount:positive_integer,
%!   +Edges:ordset(compound),
%!   -ExportGraph:compound,
%!   +Options:list(nvpair)
%! ) is det.

dh_export_graph(MaxCount, Es, ExportGraph, Options1):-
  merge_options(
    Options1,
    [
      edge_label(dh_edge_label),
      edge_penwidth(dh_edge_penwidth(MaxCount)),
      graph_directed(true),
      vertex_image(dh_vertex_image),
      vertex_label(dh_vertex_label)
    ],
    Options2
  ),
  l_edges_vertices(Es, Vs),
  build_export_graph(Vs, Es, ExportGraph, Options2).



%! dh_vertex_image(+Vertex, -CacheFile:atom) is semidet.

dh_vertex_image(V, CacheFile):-
  is_image_uri(V), !,
  rdf_global_id(V, Uri),
  uri_nested_file(data(.), Uri, CacheFile),
  (   access_file(CacheFile, exist), !
  ;   download_to_file(Uri, CacheFile, []), !
  ;   fail
  ).



%! dh_vertex_label(+Vertex, -Label:atom) is det.

dh_vertex_label(V, Label):-
  dcg_with_output_to(atom(Label), rdf_term_name([literal_ellipsis(50)], V)).
