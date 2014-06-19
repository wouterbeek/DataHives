:- module(
  dh_web_graph,
  [
    dh_web_graph/2 % +Request:list(nvpair)
                   % +Style:atom
  ]
).

/** <module> DataHives graph

@author Wouter Beek
@version 2014/04
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(generics(list_ext)).
:- use_module(xml(xml_dom)).

:- use_module(plServer(app_server)).
:- use_module(plServer(web_modules)). % Web module registration.

:- use_module(plHtml(html_pl_term)).

:- use_module(plTabular(rdf_html_table)).
:- use_module(plTabular(rdf_tabular)).

:- use_module(plGraphViz(gv_file)).

:- use_module(dh_core(dh_communication)).
:- use_module(dh_web(dh_gif)).

:- multifile(http:location/3).
:- dynamic(http:location/3).

http:location(dh_web, root(dh), []).

:- http_handler(dh_web(graph), dh_web_graph, []).

user:web_module('DH Graph', dh_web_graph).



dh_web_graph(Request):-
  dh_web_graph(Request, app_style).

dh_web_graph(_, Style):-
  findall(
    Count-[S,P,O],
    edge_count(S, P, O, Count),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  reverse(Pairs2, Pairs3),
  reply_html_page(
    Style,
    html([
      %%%%meta([content(1),'http-equiv'(refresh)], []),
      title('DataHives - Graph')
    ]),
    html([
      \dh_web_graph_graph,
      \dh_web_graph_table(Pairs3)
    ])
  ).


dh_web_graph_graph -->
  {
    dh_graph(Gif),
    graph_to_svg_dom([method(dot)], Gif, SvgDom)
  },
  html(\xml_dom_as_atom(SvgDom)).


dh_web_graph_table(Pairs1) -->
  {
    maximum_number_of_rows(MaximumNumberOfRows),
    list_truncate(Pairs1, MaximumNumberOfRows, Pairs2),
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
        \html_pl_term(_, MaximumNumberOfRows),
        ' most visited edges.'
      ]),
      [['Count','Subject','Predicate','Object']|Rows]
    )
  ).

maximum_number_of_rows(100).

