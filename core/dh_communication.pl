:- module(
  dh_communication,
  [
    default_communication/4, % +From:or([bnode,iri,literal])
                             % -Direction:oneof([backward,forward])
                             % -Link:iri
                             % -To:or([bnode,iri,literal])
    edge_count/4 % ?Subject:or([bnode,iri])
                 % ?Predicate:iri
                 % ?Object:or([bnode,iri,literal])
                 % ?Count:positive_integer
  ]
).

/** <module> DataHives communication

Communication predicates for agents in DataHives.

@author Wouter Beek
@version 2014/02, 2014/04
*/

:- dynamic(edge_count/4).



default_communication(_, _, _, _).


edge_count(From, backward, Link, To):- !,
  edge_count(To, forward, Link, From).
edge_count(From, forward, Link, To):-
  with_mutex(
    edge_count,
    (
      (
        retract(edge_count(From, Link, To, Count1))
      ->
        Count2 is Count1 + 1,
        assert(edge_count(From, Link, To, Count2))
      ;
        assert(edge_count(From, Link, To, 1))
      )
    )
  ).

