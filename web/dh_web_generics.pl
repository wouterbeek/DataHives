:- module(
  dh_web_generics,
  [
    dh_body//1, % :Body
    dh_head//1 % +Substrings:list(atom)
  ]
).

/** <module> DataHives Web: generics

Generic predicates that are reused by modules
that implement the DataHives Web-based front-end.

@author Wouter Beek
@version 2014/09, 2014/11-2014/12
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- html_meta(dh_body(html,?,?)).

:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).

user:file_search_path(css, dh(web/css)).
user:file_search_path(js, dh(web/js)).

:- http_handler(/, dh_main, [id(dh_main),prefix,priority(-10)]).



dh_main(_):-
  user:current_html_style(HtmlStyle),
  reply_html_page(HtmlStyle, \dh_head(['Main']), \dh_body([])).


dh_body(Content) -->
  html([
    \html_requires(css(pure)),
    \html_requires(js(jquery)),
    div([style='margin-left: 1.5cm; margin-top: 0.5cm;'], Content)
  ]).


dh_head(Substrings) -->
  {atomic_list_concat(['DataHives'|Substrings], ' - ', String)},
  html(title(String)).

