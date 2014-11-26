:- module(
  dh_test,
  [
    dh_test_agent/2 % +AgentDefinition:url
                    % +Initialization:or([atom,compound])
  ]
).

/** <module> DataHives tests

Simple test predicates for running programs in DataHives.

@author Wouter Beek
@author Baudouin Duthoit
@version 2013/09-2013/10, 2014/02, 2014/04-2014/07, 2014/11
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(uri)).

:- use_module(generics(meta_ext)).

:- use_module(plRdf(rdf_gc)).
:- use_module(plRdf(debug/rdf_script)).
:- use_module(plRdf(graph/rdf_graph_name)).
:- use_module(plRdf(management/rdf_load_any)).

:- use_module(plSparql(query/sparql_query_random)).

:- use_module(dh(agent/dh_agent_create)).





%! dh_test_agent(
%!   +AgentDefinition:url,
%!   +Initialization:or([atom,compound])
%! ) is det.
% Options are passed on to dh_agent_create/3.

dh_test_agent(AgentDefinition, file(File)):- !,
  uri_file_name(Uri, File),
  (   rdf_graph_property(Graph, source(Uri))
  ->  true
  ;   rdf_load_any(File, [graph(Graph)])
  ),
  dh_test_agent(AgentDefinition, graph(Graph)).
dh_test_agent(AgentDefinition, graph(Graph)):- !,
  (   var(Graph)
  ->  assert_visum(Graph)
  ;   rdf_graph(Graph)
  ),
  rdf_graph_exclude_from_gc(Graph),
  dh_agent_create(AgentDefinition, graph(Graph)).
dh_test_agent(AgentDefinition, rdf(S,P,O)):-
  default_goal(sparql_query_random_triple(dbpedia), rdf(S,P,O)),
  dh_agent_create(AgentDefinition, rdf(S,P,O)).

