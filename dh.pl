:- module(
  dh,
  [
    init_agent/4, % :Navigate
                  % :Act
                  % :Communicate
                  % +InitialResource:iri
% GRAPH PROPERTIES
    edge_count/4, % ?Subject:or([bnode,iri])
                  % ?Predicate:iri
                  % ?Object:or([bnode,iri,literal])
                  % ?Count:positive_integer
% DEFAULT ACTIONS
    some_action/4, % +From:or([bnode,iri,literal])
                   % -Direction:oneof([backward,forward])
                   % -Link:iri
                   % -To:or([bnode,iri,literal])

% DEFAUL COMMUNICATIONS
    some_communication/4 % +From:or([bnode,iri,literal])
                         % -Direction:oneof([backward,forward])
                         % -Link:iri
                         % -To:or([bnode,iri,literal])
  ]
).

/** <module> DataHives

Bzzzzz... DataHives!

@author Wouter Beek
@version 2014/02, 2014/04
*/

:- use_module(dcg(dcg_content)). % Meta-argument.
:- use_module(dcg(dcg_generic)).
:- use_module(dh(dh_lit_tag)). % Load demo.
:- use_module(library(debug)).
:- use_module(rdf(rdf_name)). % Meta-argument.

:- thread_local(backtrack/4).

:- dynamic(edge_count/4).

:- meta_predicate(init_agent(4,4,4,+)).
:- meta_predicate(nav_act_com(4,4,4,+)).



init_agent(Nav, Act, Com, Init):-
  flag(agent, Id, Id + 1),
  format(atom(Alias), 'agent_~d', [Id]),
  thread_create(nav_act_com(Nav, Act, Com, Init), _, [alias(Alias)]).


%! nav_act_com(:Navigate, :Act, :Communicate, +InitialResource:iri) .
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

nav_act_com(Nav, Act, Com, InitFrom):-
  % Initialize the backtrack option.
  assert(
    backtrack(
      InitFrom,
      forward,
      'http://www.w3.org/2002/07/owl#sameAs',
      InitFrom
    )
  ),

  repeat,

  % Navigate.
  backtrack(_, _, _, From),
  (
    call(Nav, From, Dir, Link, To)
  ->
    retract(backtrack(_, _, _, _)),
    assert(backtrack(From, Dir, Link, To))
  ;
    retract(backtrack(To, Dir0, Link, From)),
    dir_inv(Dir0, Dir),
    assert(backtrack(From, Dir, Link, To))
  ),
  with_mutex(edge_count, tick_edge_count(From, Dir, Link, To)),

  % Act.
  call(Act, From, Dir, Link, To),

  % Communicate.
  call(Com, From, Dir, Link, To),

  fail.

dir_inv(backward, forward).
dir_inv(forward, backward).


tick_edge_count(From, backward, Link, To):- !,
  tick_edge_count(To, forward, Link, From).
tick_edge_count(From, forward, Link, To):-
  retract(edge_count(From, Link, To, Count1)), !,
  Count2 is Count1 + 1,
  assert(edge_count(From, Link, To, Count2)).
tick_edge_count(From, forward, Link, To):-
  assert(edge_count(From, Link, To, 1)).



% DEFAULT ACTIONS %

some_action(From, Dir, Link, To):-
  dir_trans(Dir, Orient),
  dcg_with_output_to(atom(Arrow), arrow([head(Orient)], 4)),
  dcg_with_output_to(atom(Triple), rdf_triple_name(From, Link, To)),
  debug(dh, '~w\t~w', [Arrow,Triple]).

dir_trans(backward, left).
dir_trans(forward, right).



% DEFAULT COMMUNICATIONS %

some_communication(_, _, _, _).

