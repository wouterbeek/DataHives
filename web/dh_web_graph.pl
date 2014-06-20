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

:- use_module(plHtml(html_pl_term)).

:- use_module(plTabular(rdf_html_table)).
:- use_module(plTabular(rdf_tabular)).

:- use_module(plGraphViz(gv_file)).

:- use_module(dh_core(dh_communication)).
:- use_module(dh_web(dh_gif)).



dh_web_graph(_, Style):-
  reply_html_page(
    Style,
    % Auto-update HTML: `meta([content(1),'http-equiv'(refresh)], [])`
    html(title('DataHives - Graph')),
    html(\dh_web_graph_graph)
  ).


dh_web_graph_graph -->
  {
    dh_graph(Gif),
    graph_to_svg_dom([method(dot)], Gif, SvgDom)
  },
  html(\xml_dom_as_atom(SvgDom)).

