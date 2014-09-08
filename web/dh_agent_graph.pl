:- module(
  dh_agent_graph,
  [
    dh_agent_graph/2 % +Request:list(nvpair)
                     % +Style:atom
  ]
).

/** <module> DataHives Agent: Graph

@author Wouter Beek
@version 2014/04
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).

:- use_module(xml(xml_dom)).

:- use_module(plGraphViz(gv_file)).

:- use_module(dh_web(dh_gif)).

:- html_resource(css('gv_interactive.css'), []).
:- html_resource(js('gv_interactive.js'), []).



dh_agent_graph(_, Style):-
  reply_html_page(
    Style,
    % Auto-update HTML: `meta([content(1),'http-equiv'(refresh)], [])`
    html(\dh_agent_graph_head),
    html(\dh_agent_graph_body)
  ).

dh_agent_graph_head -->
  html([
    title('DataHives - Graph'),
    \html_requires(css('gv_interactive.css')),
    \html_requires(js('gv_interactive.js'))
  ]).

dh_agent_graph_body -->
  {
    dh_graph(Gif, []),
    gif_to_svg_dom(Gif, SvgDom, [method(dot)])
  },
  html(\xml_dom_as_atom(SvgDom)).

