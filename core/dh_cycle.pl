:- module(
  dh_cycle,
  [
    dh_cycle/4 % :Navigate
               % :Act
               % :Communicate
               % +InitialResource:iri
  ]
).

/** <module> DataHives cycle

The navigate-act-communicate cycle for agents in DataHives.

@author Wouter Beek
@version 2014/04
*/

:- use_module(dh_core(dh_navigation)).

:- meta_predicate(dh_cycle(4,4,4,+)).



%! dh_cycle(:Navigate, :Act, :Communicate, +InitialResource:iri) .
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

dh_cycle(Nav, Act, Com, InitFrom):-
  % Initialize the backtrack fact in the navigation module.
  dh_navigation_init(InitFrom),

  repeat,

  pause_after_x_steps(1000),

  % Navigate.
gtrace,
  call(Nav, From, Dir, Link, To),

  % Act.
  call(Act, From, Dir, Link, To),

  % Communicate.
  call(Com, From, Dir, Link, To),

  fail.

pause_after_x_steps(X):-
  flag(steps, Y, Y + 1),
  (
    0 =:= Y mod X
  ->
    gtrace,
    print_message(informational, steps_taken(Y))
  ;
    true
  ).



% MESSAGES

:- multifile(prolog:message).

prolog:message(steps_taken(X)) -->
  [X,' steps have been taken.',nl].

