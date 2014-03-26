:- module(
  dh_ec,
  [
    init_agent/4 % :Navigate
                 % :Act
                 % :Report
                 % :DieOrLive
                 % +InitialResource:iri
  ]
).

/** <module>

The cyclus consists of the following steps:
  1. navigate
  2. act
  3. communicate
  4. evaluate
  4. die, mutate, get born

*/



run_oracle(NumberOfAgents):-
  forall(
    between(1, NumberOfAgents, X),
    init_agent(
  ),
  

