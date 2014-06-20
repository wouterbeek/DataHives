:- module(
  dh_lod_walk_supervised,
  [
    dh_lod_walk_supervised/4 % +From:or([bnode,iri,literal])
                             % -Direction:oneof([backward,forward])
                             % -Link:iri
                             % -To:or([bnode,iri,literal])
  ]
).

/** <module> DataHives Linked Open Data walk supervised

A more informative navigation strategy for DataHives,
using the Linked Open Data stepping paradigm.

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/05-2014/06
*/

:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(random)).

:- use_module(dh_core(dh_action)).
:- use_module(dh_core(dh_communication)).
:- use_module(dh_core(dh_navigation)).
:- use_module(dh_core(dh_step)).

%! dh_lod_walk_supervised(
%!   +From:or([bnode,iri,literal]),
%!   -Direction:oneof([backward,forward]),
%!   -Link:iri,
%!   -To:or([bnode,iri,literal])
%! ) is det.

dh_lod_walk_supervised(From, Dir, Link, To):-
  dh_navigate(lod_supervised_step, From, Dir, Link, To).


%! lod_supervised_step(+Resource, -Proposition:list) is det.

lod_supervised_step(Resource, Proposition):-
  dh_step(supervised_member, Resource, Proposition, []).


%! supervised_member(-Proposition, +Propositions:list) is det.
% Return a proposition in a randomish way,
% taking the relative weights of the elements into account.
% If the list is empty it goes back
%
% ### Definition
%
% Suppose that there are edges $e_1, \ldots, e_n$,
% and the value function is denoted by $v : E \rightarrow \mathcal{N}$.
%
% We define the cumulative value $v_c$ of edge $e_i$ as
% $v_c(e_i) := \bigsum_{1 \leq j \leq i} v(e_j)$,
% for $1 \leq i \leq n$.
%
% If we pick a random number $r$ with $1 \leq r \leq v_c(e_n)$,
% then we can uniquely identify an edge $e_i$ such that
% $v_c(e_{i-1}) < r < v_c(e_{i+1})$.
%
% ### Implementation detail
%
% We consider large values first, since this will reduce
% the number recursions needed for determining the chosen edge.
%
% @see The argument order mirrors that of predicate member/2.
supervised_member(_,[]):-!,
  backtrack(_,_,_,To),
  forbide_path(To),
  thread_exit(_).
supervised_member(Proposition, [Proposition]):- !.
supervised_member(Proposition, Propositions):-
  findall(
    Value2-Proposition,
    (
      member(Proposition, Propositions),
      edge_value(Proposition, Value1),
      % Select only positive value (otherwise the count doesn't work properly)
      Value1 >= 0,
      % We add 1 to each edge value.
      % Otherwise, edges with value 0 would never be considered.
      succ(Value1, Value2)
    ),
    Pairs1
  ),

  % Process large values first.
  keysort(Pairs1, Pairs2),
  reverse(Pairs2, Pairs3),

  % Pick the random value between 1 and the cumulative edge value.
  pairs_keys(Pairs3, Keys),
  sum_list(Keys, SummedValue),
  random(0, SummedValue, Choice),

  % Choose the edge that is uniquely identified by
  % the randomly chosen number.
  supervised_member(Choice, Proposition, Pairs3).

supervised_member(Choice, Proposition, Pairs):-
  supervised_member(Choice, Proposition, 0, Pairs).

supervised_member(Choice, Proposition, Cumulative, [_-Proposition|_]):-
  Choice =< Cumulative, !.
supervised_member(Choice, Proposition, Cumulative1, [Value-_|T]):-
  Cumulative2 is Cumulative1 + Value,
  supervised_member(Choice, Proposition, Cumulative2, T).

