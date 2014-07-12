:- module(
  dh_triple_count,
  [
    top_triples_web/1, % -DOM:list
    top_triples_web/2, % +TopSize:positive_integer
                       % -DOM:list
    dh_triple_count/2 % +Triple:compound
                     % -NoStore:var
  ]
).

/** <module> Triple counter

A program that runs within the DataHives architecture and that counts triples.

@author Wouter Beek
@version 2013/10, 2014/02
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).

:- use_module(generics(list_ext)).

:- use_module(plHtml(html_table)).

:- use_module(plServer(web_modules)).

:- use_module(plRdfDev(rdf_html_table)).

http:location(dh, root(dh), []).
:- http_handler(dh(triple_count), top_triples_web, []).

user:web_module('DH TripleCount', top_triples_web).

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

top_triples_web(Request):-
  top_triples_web(Request, 10).

top_triples_web(_Request, N):-
  top_triples(FullList),
  list_truncate(FullList, N, TruncatedList),
  findall(
    [K,V],
    member(K-V, TruncatedList),
    Rows
  ),
  reply_html_page(
    app_style,
    title('DataHives - TripleCount'),
    html(
      \rdf_html_table(
        [header_row(true),indexed(true)],
        html('The top locations until now.'),
        [['Count','Triple']|Rows]
      )
    )
  ).

%! dh_triple_count(+Triple:compound, ?NoStore:atom) is det.
% Update the per-triple counter.
%
% @param Triple A compound term representing an RDF triple.
% @param NoStore Uninstantiated

dh_triple_count(Triple, _NoStore):-
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

