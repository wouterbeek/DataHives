:- module(
  dh_test,
  [
    dh_test_agent/2 % +Kind:atom
                    % +Initialization:or([atom,compound])
  ]
).

/** <module> DataHives tests

Simple test predicates for running programs in DataHives.

@author Wouter Beek
@author Baudouin Duthoit
@version 2013/09-2013/10, 2014/02, 2014/04-2014/07
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- use_module(generics(meta_ext)).

:- use_module(plRdf(rdf_gc)).
:- use_module(plRdf(rdf_graph_name)).
:- use_module(plRdf(rdf_script)).
:- use_module(plRdf_ser(rdf_serial)).

:- use_module(plSparql(sparql_random)).

:- use_module(dh_agent(dh_agent)).



%! dh_test_agent(+Kind:atom, +Initialization:or([atom,compound])) is det.
% Options are passed on to create_agent/3.

dh_test_agent(Kind, file(File)):- !,
  ensure_file_is_loaded(File, Graph),
  dh_test_agent(Kind, graph(Graph)).
dh_test_agent(Kind, graph(Graph)):- !,
  (
    var(Graph)
  ->
    assert_visum(Graph)
  ;
    rdf_graph(Graph)
  ),
  rdf_graph_exclude_from_gc(Graph),
  create_agent(Kind, graph(Graph)).
dh_test_agent(Kind, rdf(S,P,O)):-
  default_goal(sparql_random_triple(dbpedia), rdf(S,P,O)),
  create_agent(Kind, rdf(S,P,O)).



% Helpers

%! ensure_file_is_loaded(+File:atom, -Graph:atom) is det,

% The file is already loaded. Return its graph.
ensure_file_is_loaded(File, Graph):-
  uri_file_name(Uri, File),
  rdf_graph_property(Graph, source(Uri)), !.
ensure_file_is_loaded(File, Graph):-
  file_to_graph_name(File, Graph),
  rdf_load_any(File, [graph(Graph)]).

