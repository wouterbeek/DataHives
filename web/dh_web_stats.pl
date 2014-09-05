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

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(pairs)).

:- use_module(dh_stats(dh_stats)).



dh_web_stats(_, Style):-
  reply_html_page(
    Style,
    title('DataHives - Statistics'),
    html(\dh_web_stats(Goals))
  ).


dh_stat_collections(Collections):-
  aggregate_all(
    set(Goal),
    dh_stat(Goal, _, _, _),
    Goals
  ),
  dh_stat_collection

dh_stat_collection(Goal):-
  findall(
    Agent-(Time-Value),
    dh_stat(Goal, Time, Agent, Value),
    Pairs1
  ),
  group_values_by_keys(Pairs1, Pairs2),
  
