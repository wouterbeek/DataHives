:- module(
  dh_weighted_lod_walk,
  [
    dh_weighted_lod_walk/2 % -DirectedTriple:compound
                           % +Options:list(nvpair)
  ]
).

/** <module> DataHives Linked Open Data walk supervised

A more informative navigation strategy for DataHives,
using the Linked Open Data stepping paradigm.

@author Wouter Beek
@author Baudouin Duthoit
@version 2014/05-2014/07
*/

:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(random)).

:- use_module(dh_act(dh_act)).
:- use_module(dh_com(dh_communicate)).
:- use_module(dh_com(dh_edge_weight)).
:- use_module(dh_nav(dh_step)).
:- use_module(dh_nav(dh_walk)).



%! dh_weighted_lod_walk(
%!   -DirectedTriple:compound,
%!   +Options:list(nvpair)
%! ) is det.

dh_weighted_lod_walk(DirTriple, Options):-
  dh_walk(lod_weighted_step, DirTriple, Options).

%! lod_weighted_step(
%!   +Resource:or([bnode,iri,literal]),
%!   -Triple:compound,
%!   +Options:list(nvpair)
%! ) is det.

lod_weighted_step(Resource, Triple, Options):-
  dh_step(weighted_member, Resource, Triple, Options).

%! weighted_member(-Triple:compound, +Triples:list(compound)) is det.
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

weighted_member(_,[]):-!,
  backtrack(dir(_,_,_,To)),
  forbide_path(To),
  thread_exit(_).
weighted_member(Triple, [Triple]):- !.
weighted_member(Triple, Triples):-
  findall(
    EdgeWeight2-Triple,
    (
      member(Triple, Triples),
      edge_weight(Triple, EdgeWeight1),
      % Select only positive values
      % (otherwise the count does not work properly)
      EdgeWeight1 >= 0,
      % We add 1 to each edge value.
      % Otherwise, edges with value 0 would not be considered.
      EdgeWeight2 is EdgeWeight1 + 1
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
  weighted_member(Choice, Triple, Pairs3).

weighted_member(Choice, Triple, Pairs):-
  weighted_member(Choice, Triple, 0, Pairs).

weighted_member(Choice, Triple, Cumulative, [_-Triple|_]):-
  Choice =< Cumulative, !.
weighted_member(Choice, Triple, Cumulative1, [Value-_|T]):-
  Cumulative2 is Cumulative1 + Value,
  weighted_member(Choice, Triple, Cumulative2, T).

