:- module(
  dh_search,
  [
    number_of_overall_search_results/1, % -NumberOfSearchResults:nonneg
    number_of_self_search_results/1, % -NumberOfSearchResults:nonneg
    search_action/3 % +Search:compound
                    % +ResultsGraph:atom
                    % +DirectedTriple:compound
  ]
).

/** <module> DataHives search

@author Wouter Beek
@version 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(dh_agent(dh_agent)).
:- use_module(dh_core(dh_generics)).



%! number_of_overall_search_results(-NumberOfSearchResults:nonneg) is det.

number_of_overall_search_results(N):-
  aggregate_all(
    count,
    rdf(_, _, _, search_results),
    N
  ).


%! number_of_self_search_results(-NumberOfSearchResults:nonneg) is det.

number_of_self_search_results(N):-
  agent_self_graph(MyGraph),
  aggregate_all(
    count,
    rdf(_, _, _, MyGraph),
    N
  ).


%! search_action(
%!   +Search:compound,
%!   +ResultsGraph:atom,
%!   +DirectedTriple:compound
%! ) is det.

search_action(Search, ResultsGraph, DirTriple):-
  directed_triple(DirTriple, Triple),
  (
    search_result_found(Search, Triple)
  ->
    Triple = rdf(S,P,O),
    agent_self_graph(MyGraph),
    rdf_assert(S, P, O, MyGraph),
    rdf_assert(S, P, O, ResultsGraph)
  ;
    true
  ).


%! search_result_found(+Search:compound, +Triple:compound) is det.

search_result_found(instance_of(Class1), rdf(S,_,O)):-
  rdf_global_id(Class1, Class2),
  (
    rdfs_individual_of(S, Class2)
  ;
    rdfs_individual_of(O, Class2)
  ), !.

