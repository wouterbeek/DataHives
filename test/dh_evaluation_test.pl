:- module(
  dh_evaluation_test,
  [
       quantitative_evaluation/0,
       robustness_evaluation/0,
       fitness_evaluation/0,
      status/3  % Contain deaths, steps, deduction
   ]
).

:-dynamic(status/3).

quantitative_evaluation:-
  true,
  default_goal(random_start_url, Url),
  time(  % Measuring time for ants
      create_agents(
	evaluation_navigation,
	deductive_action,
	update_edge_count(1),
	fitness_evaluation,
	dh_ant_test,
	Url,
	10
      )
  ),
  time(  % Measuring time for bees
    create_agents(
      evaluation_navigation,
      default_action,
      update_edge_count(1),
      scout_evaluation,
      dh_bee_test,
      Url,
      10
    )
  ),
  true.

evaluation_navigation(From, Dir, Link, To):-
  dh_lod_walk_supervised(From, Dir, Link, To),
  retract(status(_, Steps1, _)),
  succ(Steps1,Steps2),
  assert(status(_, Steps2 , _)).

robustness_evaluation:-
  true.

fitness_evaluation:-
  true.
