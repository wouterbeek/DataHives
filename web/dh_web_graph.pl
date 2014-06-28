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

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(generics(list_ext)).
:- use_module(xml(xml_dom)).

:- use_module(plHtml(html_pl_term)).

:- use_module(plTabular(rdf_html_table)).
:- use_module(plTabular(rdf_tabular)).

:- use_module(plGraphViz(gv_file)).

:- use_module(dh_core(dh_communicate)).
:- use_module(dh_web(dh_gif)).

:- html_resource(css('gv_interactive.css'), []).
:- html_resource(js('gv_interactive.js'), []).



dh_web_graph(_, Style):-
  reply_html_page(
    Style,
    % Auto-update HTML: `meta([content(1),'http-equiv'(refresh)], [])`
    html(\dh_web_graph_head),
    html(\dh_web_graph_body)
  ).

dh_web_graph_head -->
  html([
    title('DataHives - Graph'),
    \html_requires(css('gv_interactive.css')),
    \html_requires(js('gv_interactive.js'))
  ]).

dh_web_graph_body -->
  {
    dh_graph(Gif),
    graph_to_svg_dom([method(dot)], Gif, SvgDom)
  },
  html(\xml_dom_as_atom(SvgDom)).

