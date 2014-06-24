:- module(
  dh_act,
  [
    default_action/4, % +From:or([bnode,iri,literal])
                      % -Direction:oneof([backward,forward])
                      % -Link:iri
                      % -To:or([bnode,iri,literal])
    no_action/4 % +From:or([bnode,iri,literal])
                % -Direction:oneof([backward,forward])
                % -Link:iri
                % -To:or([bnode,iri,literal])
  ]
).

/** <module> DataHives actions

Action predicates for agents in DataHives.

@author Wouter Beek
@version 2014/02, 2014/04, 2014/06
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).

:- use_module(plRdf(rdf_name)).

:- use_module(dh_core(dh_communicate)).



default_action(From, Dir, Link, To):-
  dir_trans(Dir, Orient),
  dcg_with_output_to(atom(Arrow), arrow(Orient, 4)),
  dcg_with_output_to(atom(Triple), rdf_triple_name(From, Link, To)),
  debug(dh, '~w\t~w', [Arrow,Triple]).

dir_trans(backward, left).
dir_trans(forward, right).


no_action(_, _, _, _).

