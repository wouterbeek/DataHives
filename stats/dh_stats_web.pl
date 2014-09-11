:- module(
  dh_stats_web,
  [
    dh_stats_web/2 % +Request:list(nvpair)
                   % +HtmlStyle
  ]
).

/** <module> DataHives Statistics: Web

Web-based front-end for statistics in DataHives.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_path)).

:- use_module(generics(request_ext)).

:- use_module(dh_web(dh_web_generics)).



% GET html *
dh_stats_web(Request, HtmlStyle):-
gtrace,
  cors_enable,
  request_filter(Request, get, _/html, Root),
  http_absolute_uri(dh_stats(.), Root), !,
  reply_html_page(
    HtmlStyle,
    \dh_stats_head(['']),
    html([
      p([
        'Show',
        'Y',
        'per',
        'X',
        'optional interval',
        'for',
        'agent||agent definition||population',
        'list'
      ])
    ])
  ).



% Helpers

dh_stats_head(Substrings) -->
  html(\dh_head(['Statistics'|Substrings])).

