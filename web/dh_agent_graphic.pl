:- module(
  dh_agent_graphic,
  [
    dh_agent_graphic/1 % +Request:list(nvpair)
  ]
).

/** <module> DataHives Agent: Graphic

# Auto-update

In order to auto-update / reload an HTML page insert the following:

~~~html
meta([content(1),'http-equiv'(refresh)], [])
~~~

---

@author Wouter Beek
@version 2014/04, 2014/09, 2014/11-2014/12
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).

:- use_module(plXml(xml_dom)).

:- use_module(plGraphViz(gv_file)).

:- use_module(plGraphDraw(svg_gv)).

:- use_module(dh(web/dh_gif)).
:- use_module(dh(web/dh_web_generics)).

:- html_resource(css('gv_interactive.css'), []).
:- html_resource(js('gv_interactive.js'), []).

:- http_handler(dh(graphic), dh_agent_graphic, [id(dhGraphic)]).





dh_agent_graphic(_):-
  user:current_html_style(HtmlStyle),
  reply_html_page(
    HtmlStyle,
    \dh_head(['Graphic']),
    \dh_agent_graphic_body
  ).


dh_agent_graphic_body -->
  {
    dh_graph(ExportG, []),
    export_graph_to_svg_dom(ExportG, SvgDom, [method(dot)])
  },
  html([
    \html_requires(css('gv_interactive.css')),
    \html_requires(js('gv_interactive.js')),
    \xml_dom_as_atom(SvgDom)
  ]).

