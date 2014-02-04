:- module(
  dh_traversal,
  [
    next_triple_random/2 % +FromState:compound
                         % -RandomToState:compound
  ]
).

/** <module> DataHives traversal

Methods for traversing a DataHive.

@author Wouter Beek
@version 2013/09-2013/10, 2014/02
*/

:- use_module(dh(dh_network)).
:- use_module(generics(list_ext)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(math(random_ext)).
:- use_module(rdf(rdf_read)).

:- rdf_meta(next_triple(t,t)).
:- rdf_meta(next_triple_random(t,t)).



%! next_triple(+FromState:compound, -ToState:compound) is nondet.
% Returns directly connected triples.
%
% @param FromState A compound term consisting of a hive, graph, and triple.
% @param ToState A compound term consisting of a hive, graph, and triple.

next_triple(state(H1,G1,rdf(S1,_P1,_O1)),state(H2,G2,rdf(S2,P2,O2))):-
  rdf([], S2, P2, S1, G2),
  connected(H1, G1, S1, H2, G2),
  O2 = S1.
next_triple(state(H1,G1,rdf(_S1,_P1,O1)),state(H2,G2,rdf(S2,P2,O2))):-
  rdf([], O1, P2, O2, G2),
  connected(H1, G1, O1, H2, G2),
  S2 = O1.
next_triple(state(H1,G1,rdf(_S1,P1,_O1)), state(H2,G2,rdf(S2,P2,O2))):-
  rdf([], S2, P2, P1, G2),
  connected(H1, G1, P1, H2, G2),
  O2 = P1.
next_triple(state(H1,G1,rdf(_S1,P1,_O1)), state(H2,G2,rdf(S2,P2,O2))):-
  rdf([], P1, P2, O2, G2),
  connected(H1, G1, P1, H2, G2),
  S2 = P1.

%! next_triple_random(
%!   +FromState:compound,
%!   -RandomToState:compound
%! ) is det.
% Returns a randomly chosen directly connected triple.
%
% @param FromState A compound term consisting of a hive, graph, and triple.
% @param RandomToState A compound term consisting of a hive, graph,
%        and triple.

next_triple_random(FromState, RndToState):-
  findall(ToState, next_triple(FromState, ToState), ToStates),
  random_member(RndToState, ToStates).

