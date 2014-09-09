:- module(
  dh_web_generics,
  [
    dh_body//1, % :Body
    dh_head//1 % :Subtitle
  ]
).

/** <module> DataHives Web: generics

Generic predicates that are reused by modules
that implement the DataHives Web-based front-end.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).

:- ensure_loaded(plServer(style)).

:- html_meta(dh_body(html,?,?)).
:- html_meta(dh_head(html,?,?)).



dh_body(Content) -->
  html([
    \html_requires(css(pure)),
    \html_requires(js(jquery)),
    div([style='margin-left: 1.5cm; margin-top: 0.5cm;'], Content)
  ]).


dh_head(Subtitle) -->
  html(title(['DataHives - '|Subtitle])).

