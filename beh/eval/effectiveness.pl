:- module(
  dh_effectiveness,
  [
    effectiveness/1 % -Effectiveness:float
  ]
).

/** <module> DataHives: Effectiveness

Testing a non-standard agent property.

@author Wouter Beek
@version 2014/09
*/

:- use_module(plRdf(rdf_build2)).

:- use_module(dh(agent/dh_agent_property_local)).
:- use_module(dh(beh/nav/dh_nav)).

:- initialization(init).



%! effectiveness(-Effectiveness:float) is det.

effectiveness(Effectiveness):-
  steps(Steps),
  age(Age),
  Effectiveness is Steps / Age.

init:-
  rdfs_assert_property(
    dho:effectiveness,
    dho:agentPropertyLocal,
    dho:'Agent',
    xsd:float,
    effectiveness,
    'A non-basic agent property intended for testing purposes.',
    dho
  ).
