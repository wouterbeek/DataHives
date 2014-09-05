:- module(
  dh_web_stats,
  [
    dh_web_stats/2 % +Request:list(nvpair)
                   % +Style
  ]
).

/** <module> DataHives Web Statistics

Web-based interface towards statistical measurements performed
in DataHives.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(http/html_write)).

:- use_module(dh_stats(dh_stats)).



dh_web_stats(_, Style):-
  reply_html_page(
    Style,
    title('DataHives - Statistics'),
    html(\dh_web_stats(Goals))
  ).

