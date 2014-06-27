:- module(
  dh_act,
  [
    default_action/1, % ?DirectedTriple:compound
    no_action/1 % ?DirectedTriple:compound
  ]
).

/** <module> DataHives actions

Action predicates for agents in DataHives.

@author Wouter Beek
@version 2014/02, 2014/04, 2014/06
*/

:- use_module(library(debug)).

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).

:- use_module(plRdf(rdf_name)).

:- use_module(dh_core(dh_generic)).



default_action(dir(From,Dir,Link,To)):-
  direction_translation(Dir, Orient),
  dcg_with_output_to(atom(Arrow), arrow(Orient, 4)),
  dcg_with_output_to(atom(Triple), rdf_triple_name(From, Link, To)),
  debug(dh, '~w\t~w', [Arrow,Triple]).


no_action(dir(_,_,_,_)).

