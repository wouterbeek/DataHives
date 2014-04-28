:- module(
  dh_communication,
  [
    default_communication/4 % +From:or([bnode,iri,literal])
                            % -Direction:oneof([backward,forward])
                            % -Link:iri
                            % -To:or([bnode,iri,literal])
  ]
).

/** <module> DataHives communication

Communication predicates for agents in DataHives.

@author Wouter Beek
@version 2014/02, 2014/04
*/



default_communication(_, _, _, _).

