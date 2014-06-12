:- module(
  dh_action,
  [
    default_action/4, % +From:or([bnode,iri,literal])
                     % -Direction:oneof([backward,forward])
                     % -Link:iri
                     % -To:or([bnode,iri,literal])

    supervised_action/4, % +From:or([bnode,iri,literal])
                     % -Direction:oneof([backward,forward])
                     % -Link:iri
                     % -To:or([bnode,iri,literal])

    kill_agent/0,
    forbide_path/0
  ]
).

/** <module> DataHives actions

Action predicates for agents in DataHives.

@author Wouter Beek
@version 2014/02, 2014/04
*/

:- use_module(library(debug)).

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).

:- use_module(plRdf(rdf_name)).

:- use_module(dh_core(dh_communication)).
:- use_module(dh_core(dh_navigation)).


default_action(From, Dir, Link, To):-
  dir_trans(Dir, Orient),
  dcg_with_output_to(atom(Arrow), arrow(Orient, 4)),
  dcg_with_output_to(atom(Triple), rdf_triple_name(From, Link, To)),
  debug(dh, '~w\t~w', [Arrow,Triple]).

supervised_action(From, _, 'rdf:type', 'rdf:Property'):- % Rule rdfs6
  assert_triple(From,'rdfs:SubProperty',From).
supervised_action(From, _, 'rdf:type', 'rdfs:Class'):- % Rule rdfs8
  assert_triple(From,'rdfs:SubClassOf','rdfs:Resource').
supervised_action(From, Dir, Link, To):-
  % Entailment
  assert_triple(From,'rdf:type','rdfs:Datatype'), % Rule rdfs1
  assert_triple(Link,'rdf:type','rdfs:Datatype'), % Rule rdfs1
  assert_triple(To,'rdf:type','rdfs:Datatype'), % Rule rdfs1
  assert_triple(From,'rdf:type','rdfs:Resource'), % Rule rdfs4a
  assert_triple(To,'rdf:type','rdfs:Resource'), % Rule rdfs4b
  default_action(From, Dir, Link, To).

% Will be used to store a triple (S,P,O) in the data base.
assert_triple(_,_,_):-
  true.

kill_agent:-
  halt.

forbide_path:-
  backtrack(From, _, To, Link),  % Redundancy because it goes back previously when call. We just want to know where to devalue an edge with a dead end.
  edge_count(From,Link,To,-1).

dir_trans(backward, left).
dir_trans(forward, right).

