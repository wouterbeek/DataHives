:- module(
  dh_test,
  [
    dh_test/2 % +AgentName:atom
              % +Initialization:or([atom,compound])
  ]
).

/** <module> DataHives tests

Simple test predicates for running programs in DataHives.

@author Wouter Beek
@author Baudouin Duthoit
@version 2013/09-2013/10, 2014/02, 2014/04-2014/06
*/

:- use_module(library(predicate_options)).

:- use_module(generics(meta_ext)).

:- use_module(plRdf(rdf_gc)).
:- use_module(plRdf(rdf_script)).

:- use_module(plSparql(sparql_random)).

:- use_module(dh_agent(dh_agent)).

:- predicate_options(dh_test/3, 3, [
     pass_to(create_agent/3, 3)
   ]).



%! dh_test(+Agent:atom, +Initialization:or([atom,compound])) is det.
% Options are passed on to create_agent/3.

dh_test(Agent, graph(Graph)):-
  default_goal(assert_visum, Graph),
  rdf_graph_exclude_from_gc(Graph),
  create_agent(Agent, graph(Graph)).
dh_test(Agent, rdf(S,P,O)):-
  default_goal(sparql_random_triple(dbpedia), rdf(S,P,O)),
  create_agent(Agent, rdf(S,P,O)).

