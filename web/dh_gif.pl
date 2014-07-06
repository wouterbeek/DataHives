:- module(
  dh_gif,
  [
    dh_graph/1 % -Gif:compound
  ]
).

/** <module> DataHives Graph Interchange Format

Produces descriptions of graphs in DataHives
using the Graph Interchange Format.

@author Wouter Beek
@version 2014/04-2014/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).

:- use_module(dcg(dcg_generic)).
:- use_module(generics(list_ext)).
:- use_module(generics(typecheck)).
:- use_module(generics(uri_ext)).
:- use_module(http(http_download)).
:- use_module(os(image_ext)).

:- use_module(plRdf(rdf_name)). % Meta-DCG.

:- use_module(dh_com(dh_edge_weight)).
:- use_module(dh_com(dh_com)).



%! dh_edge(-Triple:triple(or([bnode,iri,literal]))) is nondet.

dh_edge(S-P-O):-
  edge_count(rdf(S,P,O), _).


%! dh_graph(-Gif) is det.

dh_graph(Gif):-
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
  dh_graph(MaxCount, Gif).

%! dh_graph(+MaxCount:positive_integer, -Gif:compound) is det.

dh_graph(MaxCount, Gif):-
  aggregate_all(
    set(E),
    dh_edge(E),
    Es
  ),
  dh_graph(MaxCount, Es, Gif).

%! dh_graph(
%!   +MaxCount:positive_integer,
%!   +Edges:ordset(triple(or([bnode,iri,literal]))),
%!   -Gif:compound
%! ) is det.

dh_graph(MaxCount, Es, Gif):-
  edges_to_vertices(Es, Vs),
  dh_graph(MaxCount, Vs, Es, Gif).

%! dh_graph(
%!   +MaxCount:positive_integer,
%!   +Vertices:ordset(or([bnode,iri,literal])),
%!   +Edges:ordset(triple(or([bnode,iri,literal]))),
%!   -Gif:compound
%! ) is det.

dh_graph(MaxCount, Vs, Es, graph(V_Terms,E_Terms,Attrs)):-
  maplist(vertex_term(Vs), Vs, V_Terms),
  maplist(edge_term(MaxCount, Vs), Es, E_Terms),
  Attrs = [directed=true].


%! dh_vertex(-Vertex:or([bnode,iri,literal])) is nondet.

dh_vertex(V):-
  edge_count(rdf(V,_,_), _).
dh_vertex(V):-
  edge_count(rdf(_,_,V), _).


edges_to_vertices([], []):- !.
edges_to_vertices([S-_-O|T], S3):-
  edges_to_vertices(T, S1),
  ord_add_element(S1, S, S2),
  ord_add_element(S2, O, S3).



% EDGE TERMS

edge_arrow_head(_, normal).

edge_name(_-P-_, Label):-
  dcg_with_output_to(atom(Label), rdf_term_name(P)).

edge_penwidth(0, _, 1):- !.
edge_penwidth(MaxCount, S-P-O, Penwidth):-
  edge_count(rdf(S,P,O), Count),
  Penwidth is Count / MaxCount * 10.

edge_style(_, solid).

edge_term(MaxCount, Vs, E, edge(FromId,ToId,Attrs)):-
  E = From-_-To,
  nth0chk(FromId, Vs, From),
  nth0chk(ToId, Vs, To),
  edge_arrow_head(E, ArrowHead),
  edge_name(E, Label),
  edge_penwidth(MaxCount, E, Penwidth),
  edge_style(E, Style),
  Attrs = [arrowhead=ArrowHead,label=Label,penwidth=Penwidth,style=Style].



% VERTEX TERMS

vertex_image(Spec, Attrs, [image=CacheFile|Attrs]):-
  image_url(Spec), !,
  rdf_global_id(Spec, Url),
  url_nested_file(data(.), Url, CacheFile),
  (
    access_file(CacheFile, exist), !
  ;
    download_to_file(Url, CacheFile, []), !
  ;
    fail
  ).

vertex_image(_, Attrs, Attrs).

vertex_label(V, Label):-
  dcg_with_output_to(atom(Label), rdf_term_name([literal_ellipsis(50)], V)).

vertex_peripheries(_, 1).

vertex_shape(_, ellipse).

vertex_term(Vs, V, vertex(Id,V,Attrs2)):-
  nth0chk(Id, Vs, V),
  vertex_label(V, Label),
  vertex_shape(V, Shape),
  Attrs1 = [label=Label,shape=Shape],
  vertex_image(V, Attrs1, Attrs2).

