:- module(
  dh_web,
  [
    dh_web/2 % +Request:list(nvpair)
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

:- use_module(dh_agent(dh_agent)).
:- use_module(dh_agent(dh_agent_definition)).
:- use_module(dh_web(dh_web_generics)).

% /stats
:- http_handler(dh_web(stats), rest_process_stats, [prefix]).



dh_web(_, HtmlStyle):-
  reply_html_page(
    HtmlStyle,
    \dh_head(['Home']),
    \dh_web_body
  ).


dh_web_body -->
  html([
    \(dh_web_agent:dh_web_agent_body),
    \(dh_web_agentDef:dh_web_agentDef_body)
  ]).

