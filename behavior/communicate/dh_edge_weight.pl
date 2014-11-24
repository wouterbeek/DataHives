:- module(
  dh_edge_weight,
  [
    edge_count/2, % ?Triple:compound
                  % ?EdgeCount:integer
    edge_weight/2, % ?Triple:compound
                   % -EdgeWeight:integer
    reset_edge_count/0,
    update_edge_count/1, % +DirectedTripleOrTriple:compound
    update_edge_count/2 % +Update:integer
                        % +DirectedTripleOrTriple:compound
  ]
).

/** <module> DataHives: edge weights

Edge weights are implemented wiht edge counts,
where in the absence of an edge count the edge's weight is 0.

@author Wouter Beek
@author Baudouin Duthoit
@version 2014/04-2014/06
*/

:- use_module(dh(core/dh_generics)).

%! edge_count0(+Triple:compound, +Count:integer) is semidet.
%! edge_count0(+Triple:compound, -Count:integer) is semidet.
%! edge_count0(-Triple:compound, +Count:integer) is nondet.
%! edge_count0(-Triple:compound, -Count:integer) is nondet.

:- dynamic(edge_count0/2).



%! edge_count(+Triple:compound, +Count:integer) is semidet.
%! edge_count(+Triple:compound, -Count:integer) is semidet.
%! edge_count(-Triple:compound, +Count:integer) is nondet.
%! edge_count(-Triple:compound, -Count:integer) is nondet.
% Relates triples to their edge count.
%
% Edge counts implement edge weights.
% The main difference between the two is that
% edges with zero weight have no edge count.

edge_count(Triple, Count):-
  ground(Triple), !,
  edge_count0(Triple, Count), !.
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


%! reset_edge_count is det.

reset_edge_count:-
  with_mutex(edge_count, retractall(edge_count0(_,_))).


%! update_edge_count(+DirectedTripleOrTriple:compound) is det.
% Wrapper around update_edge_count/2 updating the edge count by 1.

update_edge_count(DirectedTripleOrTriple):-
  update_edge_count(1, DirectedTripleOrTriple).

%! update_edge_count(
%!   +Update:integer,
%!   +DirectedTripleOrTriple:compound
%! ) is det.

update_edge_count(N, DirectedTripleOrTriple):-
  ensure_triple(DirectedTripleOrTriple, Triple),
  with_mutex(
    edge_count,
    update_edge_count0(Triple, N)
  ).


%! update_edge_count0(+Triple:compound, +WeightUpdate:integer) is det.

update_edge_count0(Triple, N):-
  retract(edge_count0(Triple, Count1)), !,
  Count2 is Count1 + N,
  assert(edge_count0(Triple, Count2)).
update_edge_count0(Triple, N):-
  assert(edge_count0(Triple, N)).

