:- module(
  dh_aging,
  [
    aging/1 % +MaxAge:positive_integer
  ]
).

/** <module> DataHives aging

An evaluation strategy that checks whether an agent's set maximum age
has not been exceeded yet.

@author Wouter Beek
@version 2014/07, 2014/09
*/

:- use_module(dh_core(dh_cycle)). % agent_property(cycles)



%! aging(+MaxAge:positive_integer) is det.

aging(MaxAge):-
  dh:dh_agent_property(cycles, Age),
  (
    Age == MaxAge
  ->
    thread_exit(done)
  ;
    true
  ).

