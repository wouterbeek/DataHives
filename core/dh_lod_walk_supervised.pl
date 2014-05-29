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
@version 2014/05
*/

:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(random)).

:- use_module(dh_core(dh_communication)).
:- use_module(dh_core(dh_navigation)).
:- use_module(dh_core(lod_step)).



%! dh_lod_walk_supervised(
%!   +From:or([bnode,iri,literal]),
%!   -Direction:oneof([backward,forward]),
%!   -Link:iri,
%!   -To:or([bnode,iri,literal])
%! ) is det.

dh_lod_walk_supervised(From, Dir, Link, To):-
  dh_navigate(lod_supervised_step, From, Dir, Link, To).


%! lod_random_step(+Resource, -Proposition:list) is det.

lod_supervised_step(Resource, Proposition):-
  lod_step(supervised_member, Resource, Proposition).


%! supervised_member(-Proposition, +Propositions:list) is det.
% Return a proposition in a randomish way,
% taking the relative weights of the elements into account.
%
% @see The argument mirrors that of predicate member/2.

supervised_member(Proposition, [Proposition]):- !.
supervised_member(Proposition, Propositions):-
  findall(
    Value-Proposition,
    (
      member(Proposition, Propositions),
      edge_value(Proposition, Value)
    ),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  % Running up until the cumulative value exceeds the random choice
  % is faster if we consider larger values first.
  reverse(Pairs2, Pairs3),
  pairs_keys(Pairs3, Keys),
  sum_list(Keys, SummedValue),
  random(0, SummedValue, Choice),
  supervised_member(Choice, Proposition, Pairs3).

supervised_member(Choice, Proposition, Pairs):-
  supervised_member(Choice, Proposition, 0, Pairs).

supervised_member(Choice, Proposition, Cumulative, [_-Proposition|_]):-
  Choice < Cumulative, !.
supervised_member(Choice, Proposition, Cumulative1, [Value-_|T]):-
  Cumulative2 is Cumulative1 + Value,
  supervised_member(Choice, Proposition, Cumulative2, T).

