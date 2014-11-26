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
@version 2014/02, 2014/04, 2014/06, 2014/08
*/

:- use_module(library(debug)).

:- use_module(plDcg(dcg_arrow)).
:- use_module(plDcg(dcg_generics)).

:- use_module(plRdf(rdf_name)).

:- reexport(dh(beh/act/dh_entailment)).
:- reexport(dh(beh/act/dh_search)).
:- use_module(dh(core/dh_generics)).



default_action(DirectedTriple):-
  % The direction label.
  direction(DirectedTriple, Direction),
  direction_translation(Direction, Orientation),
  dcg_with_output_to(atom(Arrow), arrow(Orientation, 4)),
  
  % The triple label.
  directed_triple(DirectedTriple, Triple),
  dcg_with_output_to(atom(TripleName), rdf_triple_name(Triple)),
  
  % Emit the direction+triple labels.
  debug(dh, '~w\t~w', [Arrow,TripleName]).

direction_translation(backward, left).
direction_translation(forward, right).


no_action(dir(_,_,_,_)).

