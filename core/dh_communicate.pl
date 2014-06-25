:- module(
  dh_communicate,
  [
    no_communication/4, % +From:or([bnode,iri,literal])
                        % -Direction:oneof([backward,forward])
                        % -Link:iri
                        % -To:or([bnode,iri,literal])
    forbide_path/1 % +From:or([bnode,iri,literal])
  ]
).

/** <module> DataHives communication

Communication predicates for agents in DataHives.

@author Wouter Beek
@author Baudouin Duthoit
@version 2014/02, 2014/04-2014/05
*/

:- use_module(library(ansi_term)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).

:- use_module(dh_com(dh_edge_weight)).



no_communication(_, _, _, _).


% In case of dead ends, we must tell the others about the non viability
% of the path. Unfortunately (or fortunately), this function seems not
% being used.

forbide_path(To):-
  single_alley(To,Alley),
  ansi_format([italic,fg(red)],'~w',['WrongPath\n']),
  devalue(Alley).

single_alley(Next, [[Prev,Link,Next]|Alley]):-
  single_step(Prev, Link, Next),
  single_alley(Prev, Alley).
single_alley(_, []).

single_step(Prev, Link, Next):-
  rdf(Prev, Link, Next),
  \+ ((
    rdf(Prev, Link0, Next0),
    Link0 \== Link,
    Next0 \== Next
  )).

%! devalue(+Path:list(compound)) is det.
% Value of triple S-P-O decreased by one

devalue(Triples):-
  maplist(update_edge_count(-1), Triples).
