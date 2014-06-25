:- module(
  dh_bee_fly,
  [
% SCOUT
    dh_bee_lod_fly/4 % -From
                     % +Dir
                     % +Link
                     % +To
  ]
).

:- use_module(dh_test(dh_test_generics)).
:- use_module(dh_core(dh_navigate)).
:- use_module(dh_nav(dh_step)).

dh_bee_lod_fly(From, Dir, Link, To):-
  dh_navigate(random_fly_step, From, Dir, Link, To).

random_fly_step(Resource, Proposition):-
  dh_step(get_random_proposition, Resource, Proposition, []).

get_random_proposition(rdf(S,P,S), _):-
  rdf_global_id(owl:sameAs, P),
  random_start_url(S).

