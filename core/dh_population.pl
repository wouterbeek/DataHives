:- module(
  dh_population,
  [
    total_lifetime/1, % -TotalLifetime:nonneg
    number_of_deduced_triples/1 % -TotaNumberOfDeducedTriples:nonneg
  ]
).

/** <module> DataHives: population

Population statistics for DataHives

@author Wouter Beek
@version 2014/06
*/

:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).


%! total_lifetime(-TotalLifetime:nonneg) is det.

total_lifetime(TotalLifetime):-
  thread_self(Me),
  findall(
    Thread,
    (
      agent_thread(Thread),
      thread_send_message(Thread, get_lifetime(Me))
    ),
    Threads
  ),
  collect_answers(Threads, Lifetimes),
  sum_list(Lifetimes, TotalLifetime).


collect_answers(Threads, Answers):-
  collect_answers(Threads, [], Answers).

collect_answers([], Solution, Solution):- !.
collect_answers(Threads1, Answers, Solution):-
  thread_get_message(lifetime(Thread,Answer)),
  selectchk(Thread, Threads1, Threads2), !,
  collect_answers(Threads2, [Answer|Answers], Solution).
collect_answers(Threads, Answers, Solution):-
  sleep(1),
  collect_answers(Threads, Answers, Solution).


agent_thread(Thread):-
  thread_property(Thread, status(_)),
  agent_thread_name(Thread).

agent_thread_name(Thread):-
  atom_concat('agent_', _, Thread).


%! number_of_deduced_triples(-TotaNumberOfDeducedTriples:nonneg) is det

number_of_deduced_triples(T):-
  findall(
    T,
    (
      rdf_graph(G),
      atom_concat('agent_',_,G),
      rdf_statistics(triples_by_graph(G,T))
    ),
    Ts
  ),
  sum_list(Ts,T).
