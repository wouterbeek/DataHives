:- module(
  dh_agent_graphic,
  [
    dh_agent_graphic/2 % +Request:list(nvpair)
                       % +Style:atom
  ]
).

/** <module> DataHives Agent: Graphic

@author Wouter Beek
@version 2014/04, 2014/09
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).

:- use_module(xml(xml_dom)).

:- use_module(plGraphViz(gv_file)).

:- use_module(dh(web/dh_gif)).
:- use_module(dh(web/dh_web_generics)).

:- html_resource(css('gv_interactive.css'), []).
:- html_resource(js('gv_interactive.js'), []).



dh_agent_graphic(_, HtmlStyle):-
  reply_html_page(
    HtmlStyle,
    % Auto-update HTML: `meta([content(1),'http-equiv'(refresh)], [])`
    \dh_head(['Graphic']),
    \dh_agent_graphic_body
  ).


dh_agent_graphic_body -->
  {
    dh_graph(Gif, []),
    gif_to_svg_dom(Gif, SvgDom, [method(dot)])
  },
  html([
    \html_requires(css('gv_interactive.css')),
    \html_requires(js('gv_interactive.js')),
    \xml_dom_as_atom(SvgDom)
  ]).

