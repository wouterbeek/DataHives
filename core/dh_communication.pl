:- module(
  dh_communication,
  [
    default_communication/4, % +From:or([bnode,iri,literal])
                             % -Direction:oneof([backward,forward])
                             % -Link:iri
                             % -To:or([bnode,iri,literal])
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
    update_edge_count/4, % +From:or([bnode,iri,literal])
                        % +Direction:oneof([backward,forward])
                        % +Link:iri
                        % +To:or([bnode,iri,literal])

    update_edge_count/5, % +From:or([bnode,iri,literal])
                        % +Direction:oneof([backward,forward])
                        % +Link:iri
                        % +To:or([bnode,iri,literal])
                        % +N:int
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



default_communication(_, _, _, _).



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


%! update_edge_count(
%!   +From:or([bnode,iri,literal]),
%!   +Direction:oneof([backward,forward]),
%!   +Link:iri,
%!   +To:or([bnode,iri,literal]),
%!   +N:int
%! ) is det.
update_edge_count(From, Dir, Link, To):-
  update_edge_count(From, Dir, Link, To, 1).
update_edge_count(From, backward, Link, To,N):- !,
  update_edge_count(To, forward, Link, From,N).
update_edge_count(From, forward, Link, To,N):-
  with_mutex(
    edge_count,
    update_edge_count0(From, Link, To,N)
  ).

update_edge_count0(From, Link, To,N):-
  retract(edge_count0(From, Link, To, Count1)), !,
  Count2 is Count1 + N,
  assert(edge_count0(From, Link, To, Count2)).
update_edge_count0(From, Link, To,_):-
  assert(edge_count0(From, Link, To, 1)).


% In case of dead ends, we must tell the others about the non viability
% of the path. Unfortunately (or fortunately), this function seems not
% being used.

forbide_path(To):-
  single_alley(To,Alley),
  ansi_format([italic,fg(red)],'~w','Forbiden Path'),
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

% Value of triple S-P-O decreased by one
%
devalue([]):-!.
devalue([[S,P,O]|T]):-
  update_edge_count(S,forward,P,O,-1),
  devalue(T).
