:- module(
  dh_edge_weight,
  [
    edge_count/2, % ?Triple:compound
                  % ?EdgeCount:integer
    edge_weight/2, % ?Triple:compound
                   % -EdgeWeight:integer
    update_edge_count/2, % +Update:integer
                         % +Triple:compound
    update_edge_count/5 % +Update:integer
                        % +From:or([bnode,iri,literal])
                        % +Direction:oneof([backward,forward])
                        % +Link:iri
                        % +To:or([bnode,iri,literal])
  ]
).

/** <module> DataHives: edge weights

Edge weights are implemented wiht edge counts,
where in the absence of an edge count the edge's weight is 0.

@author Wouter Beek
@author Baudouin Duthoit
@version 2014/04-2014/06
*/

%! edge_count0(+Triple:compound, +Count:integer) is semidet.
%! edge_count0(+Triple:compound, -Count:integer) is semidet.
%! edge_count0(-Triple:compound, +Count:integer) is nondet.
%! edge_count0(-Triple:compound, -Count:integer) is nondet.

:- dynamic(edge_count0/2).



%! edge_count(+Triple:compound, +Count:integer) is semidet.
%! edge_count(+Triple:compound, -Count:integer) is semidet.
%! edge_count(-Triple:compound, +Count:integer) is nonedet.
%! edge_count(-Triple:compound, -Count:integer) is nondet.
% Relates triples to their edge count.
%
% Edge counts implement edge weights.
% The main difference between the two is that
% edges with zero weight have no edge count.

edge_count(rdf(S,P,O), Count):-
  maplist(nonvar, [S,P,O]), !,
  edge_count0(rdf(S,P,O), Count), !.
edge_count(Triple, Count):-
  edge_count0(Triple, Count).


%! edge_weight(+Triple:compound, +EdgeWeight:nonneg) is semidet.
%! edge_weight(+Triple:compound, -EdgeWeight:nonneg) is det.
%! edge_weight(-Triple:compound, +EdgeWeight:nonneg) is nondet.
%! edge_weight(-Triple:compound, -EdgeWeight:nonneg) is nondet.
% Relates triples to their edge weight.
%
% The semantics of the edge weight are agent specific.
% E.g., the edge weight may reflect how popular/unpopular an edge is,
% how important it is for deduction, or how much time it takes
% to retrieve it from the Internet.

edge_weight(Triple, Count):-
  edge_count(Triple, Count), !.
edge_weight(_, 0).


%! update_edge_count(+Update:integer, +Triple:compound) is det.

update_edge_count(N, rdf(S,P,O)):-
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
    update_edge_count0(rdf(From,Link,To), N)
  ).


%! update_edge_count0(+Triple:compound, +WeightUpdate:integer) is det.

update_edge_count0(rdf(From,Link,To), N):-
  retract(edge_count0(rdf(From,Link,To), Count1)), !,
  Count2 is Count1 + N,
  assert(edge_count0(rdf(From,Link,To), Count2)).
update_edge_count0(rdf(From,Link,To), N):-
  assert(edge_count0(rdf(From,Link,To), N)).

