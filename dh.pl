:- module(
  dh,
  [
    nav_act_com/4, % :Navigate
                   % :Act
                   % :Communicate
                   % +InitialResource:iri
    some_action/4, % +From:or([bnode,iri,literal])
                   % -Direction:oneof([backward,forward])
                   % -Link:iri
                   % -To:or([bnode,iri,literal])
    some_communication/4 % +From:or([bnode,iri,literal])
                         % -Direction:oneof([backward,forward])
                         % -Link:iri
                         % -To:or([bnode,iri,literal])
  ]
).

/** <module> DataHives

Bzzzzz... DataHives!

@author Wouter Beek
@version 2014/02
*/

:- use_module(dcg(dcg_content)). % Meta-argument.
:- use_module(dcg(dcg_generic)).
:- use_module(rdf(rdf_name)). % Meta-argument.



% nav_act_com(:Navigate, :Act, :Communicate, +InitialResource:iri) .
% Implementation of the navigate-act-communicate cycle.
%
% When the walker visits a blank node or a literal,
%  it is impossible to use the LOD walker
%  since literals and blank nodes give no information
%  as to where on the Web we may find linked data.
% The `Backtrack` parameter is instatiated with the last traversal,
%  which is the node that will be visited after a literal or blank node
%  was visited.
% The same is true for any term that does not dereference,
%  such as non-dereferencing URLs.
%
% @tbd How to properly traverse nodes that are literals?
% @tbd How to properly traverse nodes that are blank nodes?

:- meta_predicate(nav_act_com(4,4,4,+)).
nav_act_com(Nav, Act, Com, InitFrom):-
  nav_act_com(
    Nav,
    Act,
    Com,
    [[InitFrom,'http://www.w3.org/2002/07/owl#sameAs',InitFrom]],
    InitFrom
  ).

:- meta_predicate(nav_act_com(5,4,4,+,+)).
nav_act_com(Nav, Act, Com, [H|_], From):-
  % Navigate.
  (
    call(Nav, From, Dir, Link, To), !
  ;
    H = [To,Dir0,Link,From],
    dir_inv(Dir0, Dir)
  ),

  % Act.
  call(Act, From, Dir, Link, To),

  % Communicate.
  call(Com, From, Dir, Link, To),

  % Recurse.
  nav_act_com(Nav, Act, Com, [[From,Dir,Link,To]], To).


some_action(From, Dir, Link, To):-
  dir_trans(Dir, Orient),
  dcg_with_output_to(user_output, arrow([head(Orient)], 4)),
  tab(user_output, 1),
  dcg_with_output_to(user_output, rdf_triple_name(From, Link, To)),
  nl(user_output),
  flush_output(user_output).

dir_inv(backward, forward).
dir_inv(forward, backward).

dir_trans(backward, left).
dir_trans(forward, right).

some_communication(_, _, _, _).

