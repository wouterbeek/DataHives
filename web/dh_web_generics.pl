:- module(
  dh_web_generics,
  [
    dh_head//1 % :Subtitle
  ]
).

/** <module> DataHives Web: generics

Generic predicates that are reused by modules
that implement the DataHives Web-based front-end.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(http/html_write)).

:- use_module(plDcg(dcg_meta)).

:- html_meta(dh_head(html,?,?)).



dh_head(Subtitle) -->
  html(title(['DataHives - '|Subtitle])).

