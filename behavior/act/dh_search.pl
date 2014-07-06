:- use_module(
  dh_search,
  [
    search_action/2, % +Class:iri
                            % +DirectedTriple:compound
  ]
).

/** <module> DataHives search

@author Wouter Beek
@version 2014/07
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(flag_ext)).

:- use_module(dh_agent(dh_agent)).
:- use_module(dh_core(dh_generics)).



report_search_results(Initialization):-
  thread_flag(number_of_search_results, N),
  default_exit(Initialization),
  


%! search_action(+Search:compound, +DirectedTriple:compound) is det.

search_action(Search, DirectedTriple):-
  directed_triple(DirTriple, Triple),
  (
    search_result_found(Search, Triple)
  ->
    thread_flag(number_of_search_results, N, N + 1)
  ;
    true
  ).


%! search_result_found(+Search:compound, +Triple:compound) is det.

search_result_found(instance_of(Class1), rdf(S,_,O)):-
  rdf_global_id(Class1, Class2),
  rdfs_individual_of(S, Class2),
  rdfs_individual_of(O, Class2).

