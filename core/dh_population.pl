:- module(
  dh_population,
  [
    dh_agent_thread/1, % ?Thread:atom
    exit_population/0,
    number_of_deduced_triples/1, % -TotaNumberOfDeducedTriples:nonneg
    total_number_of_cycles/1, % -Cycles:nonneg
    total_number_of_steps/1 % -Steps:nonneg
  ]
).

/** <module> DataHives: population

Population statistics for DataHives

@author Wouter Beek
@version 2014/06
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(thread_ext)).
:- use_module(generics(vox_populi)).

:- use_module(dh_com(dh_edge_weight)).



dh_agent_prefix(agent_).


%! dh_agent_thread(+Thread:atom) is semidet.
%! dh_agent_thread(-Thread:atom) is nondet.

dh_agent_thread(Thread):-
  dh_agent_prefix(Prefix),
  thread_prefix(Prefix, Thread).


%! exit_population is det.

exit_population:-
  dh_agent_prefix(Prefix),
  command_thread_prefix(Prefix, thread_exit(true)),
  reset_edge_count.


%! total_number_of_cycles(-Cycles:nonneg) is det.

total_number_of_cycles(Cycles):-
  dh_agent_prefix(Prefix),
  ask_thread_prefix(Prefix, number_of_cycles, Cycless),
  sum_list(Cycless, Cycles).


%! total_number_of_steps(-Steps:nonneg) is det.

total_number_of_steps(Steps):-
  dh_agent_prefix(Prefix),
  ask_thread_prefix(Prefix, number_of_steps, Stepss),
  sum_list(Stepss, Steps).


%! number_of_deduced_triples(-TotaNumberOfDeducedTriples:nonneg) is det

number_of_deduced_triples(Triples):-
  aggregate_all(
    sum(Triples),
    (
      rdf_graph(Graph),
      dh_agent_thread(Graph),
      rdf_statistics(triples_by_graph(Graph,Triples))
    ),
    Triples
  ).

