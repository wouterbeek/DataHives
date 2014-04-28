:- module(dh_web_graph, []).

/** <module> DataHives graph

@author Wouter Beek
@version 2014/04
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(generics(list_ext)).
:- use_module(pl_web(html_pl_term)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(server(web_modules)).

:- use_module(dh_core(dh_communication)).

http:location(dh_web, root(dh), []).
:- http_handler(dh_web(graph), dh_web_graph, []).

user:web_module('DH Graph', dh_web_graph).



dh_web_graph(_Request):-
  findall(
    Count-[S,P,O],
    edge_count(S, P, O, Count),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  reverse(Pairs2, Pairs3),
  number_of_rows(NumberOfRows),
  list_truncate(Pairs3, NumberOfRows, Pairs4),
  findall(
    [Count|Triple],
    member(Count-Triple, Pairs4),
    Rows
  ),
  reply_html_page(
    app_style,
    title('DataHives - Graph'),
    html(
      \rdf_html_table(
        [header_row(true)],
        html([
          'Ranking of the ',
          \html_pl_term(NumberOfRows),
          ' most visited edges.'
        ]),
        [['Count','Subject','Predicate','Object']|Rows]
      )
    )
  ).

number_of_rows(100).

