:- module(dh_web_graph, []).

/** <module> DataHives graph

penwidth

@author Wouter Beek
@version 2014/04
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(dcg(dcg_generic)).
:- use_module(generics(list_ext)).
:- use_module(gv(gv_file)).
:- use_module(pl_web(html_pl_term)).
:- use_module(rdf(rdf_name)). %
:- use_module(rdf_web(rdf_html_table)).
:- use_module(server(app_ui)).
:- use_module(server(web_modules)). %
:- use_module(xml(xml_dom)).

:- use_module(dh_core(dh_communication)).

http:location(dh_web, root(dh), []).
:- http_handler(dh_web(graph), dh_web_graph, []).

user:web_module('DH Graph', dh_web_graph).



dh_edge(S-P-O):-
  edge_count(S, P, O, _).

dh_graph(MaxCount, Gif):-
  aggregate_all(set(V), dh_vertex(V), Vs),
  aggregate_all(set(E), dh_edge(E), Es),
  dh_graph(MaxCount, Vs, Es, Gif).

dh_graph(MaxCount, Vs, Es, graph(V_Terms,E_Terms,Attrs)):-
  maplist(vertex_term(Vs), Vs, V_Terms),
  maplist(edge_term(MaxCount, Vs), Es, E_Terms),
  Attrs = [dir(forward)].

dh_vertex(V):-
  edge_count(V, _, _, _).
dh_vertex(V):-
  edge_count(_, _, V, _).


edge_arrow_head(_, normal).

edge_name(_-P-_, Label):-
  dcg_with_output_to(atom(Label), rdf_term_name(P)).

edge_penwidth(MaxCount, S-P-O, Penwidth):-
  edge_count(S, P, O, Count),
  Penwidth is ceiling(Count / MaxCount * 10).

edge_style(_, solid).

edge_term(MaxCount, Vs, E, edge(FromId,ToId,Attrs)):-
  E = From-_-To,
  nth0chk(FromId, Vs, From),
  nth0chk(ToId, Vs, To),
  edge_arrow_head(E, ArrowHead),
  edge_name(E, Label),
  edge_penwidth(MaxCount, E, Penwidth),
  edge_style(E, Style),
  Attrs = [arrowhead(ArrowHead),label(Label),penwidth(Penwidth),style(Style)].

vertex_peripheries(_, 1).

vertex_shape(_, ellipse).

vertex_term(Vs, V, vertex(Id,V,Attrs)):-
  nth0chk(Id, Vs, V),
  dcg_with_output_to(atom(Label), rdf_term_name(V)),
  vertex_shape(V, Shape),
  Attrs = [label(Label),shape(Shape)].


dh_web_graph(_Request):-
  findall(
    Count-[S,P,O],
    edge_count(S, P, O, Count),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  reverse(Pairs2, Pairs3),
  (
    Pairs3 == []
  ->
    MaxCount = 1
  ;
    Pairs3 = [MaxCount-_|_]
  ),
  reply_html_page(
    app_style,
    title('DataHives - Graph'),
    html([
      h2('Graph'),
      \dh_web_graph_graph(MaxCount),
      h2('Table'),
      \dh_web_graph_table(Pairs3)
    ])
  ).

dh_web_graph_graph(MaxCount) -->
  {
    dh_graph(MaxCount, Gif),
    graph_to_svg_dom([method(dot)], Gif, SvgDom)
  },
  html(\xml_dom_as_atom(SvgDom)).

dh_web_graph_table(Pairs1) -->
  {
    number_of_rows(NumberOfRows),
    list_truncate(Pairs1, NumberOfRows, Pairs2),
    findall(
      [Count|Triple],
      member(Count-Triple, Pairs2),
      Rows
    )
  },
  html(
    \rdf_html_table(
      [header_row(true)],
      html([
        'Ranking of the ',
        \html_pl_term(NumberOfRows),
        ' most visited edges.'
      ]),
      [['Count','Subject','Predicate','Object']|Rows]
    )
  ).

number_of_rows(100).

