:- module(
  dh_search,
  [
    search_action/3 % +Search:compound
                    % +ResultsGraph:atom
                    % +DirectedTriple:compound
  ]
).

/** <module> DataHives search

@author Wouter Beek
@version 2014/07
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(dh_core(dh_generics)).



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
    thread_self(Me),
    thread_property(Me, alias(MyName)),
    Triple = rdf(S,P,O),
    rdf_assert(S, P, O, MyName),
    rdf_assert(S, P, O, ResultsGraph)
  ;
    true
  ).


%! search_result_found(+Search:compound, +Triple:compound) is det.

search_result_found(instance_of(Class1), rdf(S,_,O)):-
  rdf_global_id(Class1, Class2),
  rdfs_individual_of(S, Class2),
  rdfs_individual_of(O, Class2).

