:- module(
  dh_communication,
  [
    edge_count/4, % ?Subject:or([bnode,iri])
                  % ?Predicate:iri
                  % ?Object:or([bnode,iri,literal])
                  % ?Count:positive_integer
    edge_value/2, % +Proposition:list(list)
                  % -Value:nonneg
    edge_value/4, % +Subject:or([bnode,iri])
                  % +Predicate:iri
                  % +Object:or([bnode,iri,literal])
                  % -Value:nonneg
    no_communication/4, % +From:or([bnode,iri,literal])
                        % -Direction:oneof([backward,forward])
                        % -Link:iri
                        % -To:or([bnode,iri,literal])
    update_edge_count/5, % +Update:integer
                         % +From:or([bnode,iri,literal])
                         % +Direction:oneof([backward,forward])
                         % +Link:iri
                         % +To:or([bnode,iri,literal])
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

:- dynamic(edge_count0/4).



%! edge_count(+Proposition:list, -Count:positive_integer) is semidet.
%! edge_count(?Proposition:list, ?Count:positive_integer) is nondet.

edge_count([S,P,O], Count):-
  edge_count(S, P, O, Count).

%! edge_count(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal]),
%!   ?Count:positive_integer
%! ) is semidet.
% Returns the edge count of the given triple.
% This fails for triples with count 0.
%! edge_count(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,iri,literal]),
%!   ?Count:positive_integer
%! ) is nondet.
% Enumerates triples with their edge count,
% possibly constrained by terms in the triple and/or the count.
% This does *not* include triples with count 0.

edge_count(S, P, O, Count):-
  maplist(nonvar, [S,P,O]), !,
  edge_count0(S, P, O, Count), !.
edge_count(S, P, O, Count):-
  edge_count0(S, P, O, Count).


%! edge_value(+Proposition:list, -Value:nonneg) is det.

edge_value([S,P,O], Value):-
  edge_value(S, P, O, Value).

%! edge_value(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal]),
%!   -Value:nonneg
%!  ) is det.
% Similar to edge_count/4 with instantiation `(+,+,+,-)`,
% but returns 0 for triples with no edge count.

edge_value(S, P, O, Count):-
  edge_count(S, P, O, Count), !.
edge_value(_, _, _, 0).


no_communication(_, _, _, _).


%! update_edge_count(
%!   +Update:integer,
%!   +Triple:list(or([bnode,iri,literal]))
%! ) is det.

update_edge_count(N, [S,P,O]):-
  update_edge_count(N, S, forward, P, O).

%! update_edge_count(
%!   +Update:integer,
%!   +From:or([bnode,iri,literal]),
%!   +Direction:oneof([backward,forward]),
%!   +Link:iri,
%!   +To:or([bnode,iri,literal])
%! ) is det.

update_edge_count(N, From, backward, Link, To):- !,
  update_edge_count(N, To, forward, Link, From).
update_edge_count(N, From, forward, Link, To):-
  with_mutex(
    edge_count,
    update_edge_count0(From, Link, To, N)
  ).

update_edge_count0(From, Link, To, N):-
  retract(edge_count0(From, Link, To, Count1)), !,
  Count2 is Count1 + N,
  assert(edge_count0(From, Link, To, Count2)).
update_edge_count0(From, Link, To, N):-
  assert(edge_count0(From, Link, To, N)).


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

%! devalue(+Path:list(list(or([bnode,iri,literal])))) is det.
% Value of triple S-P-O decreased by one

devalue(Triples):-
  maplist(update_edge_count(-1), Triples).

