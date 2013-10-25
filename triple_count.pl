:- module(
  triple_count,
  [
    top_triples_web/1, % -DOM:list
    top_triples_web/2, % +TopSize:positive_integer
                       % -DOM:list
    triple_count/2 % +Triple:compound
                   % -NoStore:var
  ]
).

/** <module> Triple counter

A program that runs within the DataHives architecture and that counts triples.

@author Wouter Beek
@version 2013/10
*/

:- use_module(html(html_table)).
:- use_module(library(lists)).
:- use_module(server(web_console)).

:- register_module(triple_count).

%! triple_count(?Pairs:list(nvpair)) is det.

:- dynamic(triple_count/1).

:- initialization(init_triple_count).



init_triple_count:-
  (
    triple_count(_), !
  ;
    assert(triple_count([]))
  ),
  (
    thread_property(_Id, alias(triple_count_manager)), !
  ;
    thread_create(triple_count_manager, _Id, [alias(triple_count_manager)])
  ).

top_triples(A3):-
  triple_count(A1),
  keysort(A1, A2),
  reverse(A2, A3).

top_triples_web(Markup):-
  top_triples_web(10, Markup).

top_triples_web(N, [HTML_Table]):-
  top_triples(L0),
  length(L1, N),
  append(L1, _, L0),
  findall(
    [K,V],
    member(K-V, L1),
    Rows
  ),
  html_table(
    [
      caption('The top locations until now.'),
      header(true),
      indexed(true)
    ],
    [['Count','Triple']|Rows],
    HTML_Table
  ).

%! triple_count(+Triple:compound, ?NoStore:atom) is det.
% Update the per-triple counter.
%
% @param Triple A compound term representing an RDF triple.
% @param NoStore Uninstantiated

triple_count(Triple, _NoStore):-
  thread_send_message(triple_count_manager, triple_count(Triple)).

triple_count_manager:-
  thread_get_message(triple_count(Triple)),
  
  retract(triple_count(A1)),
  
  (
    select(I1-Triple, A1, A2)
  ->
    I2 is I1 + 1,
    A3 = [I2-Triple|A2]
  ;
    A3 = [1-Triple|A1]
  ),
  
  assert(triple_count(A3)),
  
  triple_count_manager.

