:- module(
  dh_web,
  [
    dh/2 % +Request:list(nvpair)
         % +HtmlStyle
  ]
).

/** <module> DataHives Web

The Web API for DataHives.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(dh(agent/dh_agent)).
:- use_module(dh(agent/definition/dh_agent_definition)).
:- use_module(dh(web/dh_web_generics)).

% /stats
:- http_handler(dh(stats), rest_process_stats, [prefix]).



dh(_, HtmlStyle):-
  reply_html_page(
    HtmlStyle,
    \dh_head(['Home']),
    html(dummy)
  ).

