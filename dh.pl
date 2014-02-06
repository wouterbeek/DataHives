:- module(
  dh,
  [
    init_agent/4, % :Navigate
                  % :Act
                  % :Communicate
                  % +InitialResource:iri

% DEFAULT ACTIONS
    some_action/5, % +Alias:atom
                   % +From:or([bnode,iri,literal])
                   % -Direction:oneof([backward,forward])
                   % -Link:iri
                   % -To:or([bnode,iri,literal])

% DEFAUL COMMUNICATIONS
    some_communication/5 % +Alias:atom
                         % +From:or([bnode,iri,literal])
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
:- use_module(library(debug)).
:- use_module(rdf(rdf_name)). % Meta-argument.


:- meta_predicate(init_agent(5,5,5,+)).
init_agent(Nav, Act, Com, Init):-
  flag(agent, Id, Id + 1),
  format(atom(Alias), 'agent_~d', [Id]),
  thread_create(nav_act_com(Alias, Nav, Act, Com, Init), _, [alias(Alias)]).


%! nav_act_com(
%!   +Alias:atom,
%!   :Navigate,
%!   :Act,
%!   :Communicate,
%!   +InitialResource:iri
%! ) .
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

:- thread_local(backtrack/5).
:- meta_predicate(nav_act_com(+,5,5,5,+)).
nav_act_com(Alias, Nav, Act, Com, InitFrom):-
  % Initialize the backtrack option.
  assert(
    backtrack(
      Alias,
      InitFrom,
      forward,
      'http://www.w3.org/2002/07/owl#sameAs',
      InitFrom
    )
  ),

  repeat,

  % Navigate.
  backtrack(Alias, _, _, _, From),
  (
    call(Nav, Alias, From, Dir, Link, To)
  ->
    retract(backtrack(Alias, _, _, _, _)),
    assert(backtrack(Alias, From, Dir, Link, To))
  ;
    retract(backtrack(Alias, To, Dir0, Link, From)),
    dir_inv(Dir0, Dir),
    assert(backtrack(Alias, From, Dir, Link, To))
  ),

  % Act.
  call(Act, Alias, From, Dir, Link, To),

  % Communicate.
  call(Com, Alias, From, Dir, Link, To),

  fail.

dir_inv(backward, forward).
dir_inv(forward, backward).



% DEFAULT ACTIONS %

some_action(_, From, Dir, Link, To):-
  dir_trans(Dir, Orient),
  dcg_with_output_to(atom(Arrow), arrow([head(Orient)], 4)),
  dcg_with_output_to(atom(Triple), rdf_triple_name(From, Link, To)),
  debug(dh, '~w\t~w', [Arrow,Triple]).
  %format(user_output, '~w\t~w\n', [Arrow,Triple]).

dir_trans(backward, left).
dir_trans(forward, right).



% DEFAULT COMMUNICATIONS %

some_communication(_, _, _, _, _).

