:- module(
  dh_evaluation_test,
  [
    evaluation/2 % +AgentName:atom
                 % +NumberOfAgents:nonneg
  ]
).


/** <module> DataHives: evluation_test

Evaluation / comparison of the different kind of agents

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06-2014/07
*/

:- use_module(dh_test(dh_test)).



%! evaluation(+AgentName:atom, +NumberOfAgents:nonneg) is det.
% Starts an evaluation of the agents.

evaluation(Agent, N):-
  gtrace, %DEB
  absolute_file_name(data('myMediumDB.nt'), File, [access(read)]),
  forall(
  	between(1,N,_),
  	dh_test_agent(Agent, file(File))
  ).


