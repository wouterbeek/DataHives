:- module(
  dh_cycle,
  [
    dh_cycle/5 % :Navigate
               % :Act
               % :Communicate
               % :Evaluate
               % +InitialResource:iri
  ]
).

/** <module> DataHives cycle

The navigate-act-communicate cycle for agents in DataHives.

@author Wouter Beek
@version 2014/04-2014/06
*/

:- use_module(generics(meta_ext)).

:- use_module(dh_core(dh_navigation)).

:- meta_predicate(dh_cycle(4,4,4,0,+)).



% ! dh_cycle(:Navigate, :Act, :Communicate, :Evaluate, +InitialResource:iri) .
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

dh_cycle(Nav, Act, Com, Eval, InitFrom):-
  % Initialize the backtrack fact in the navigation module.
  dh_navigation_init(InitFrom),

  repeat,

  after_n_steps(100, print_steps),

  % Navigate.
%catch(
  call(Nav, From, Dir, Link, To),
%_, (gtrace, call(Nav, From, Dir, Link, To))),

  % Act.
%catch(
  call(Act, From, Dir, Link, To),
%_, (gtrace, call(Nav, From, Dir, Link, To))),

  % Communicate.
%catch(
  call(Com, From, Dir, Link, To),
%_, (gtrace, call(Nav, From, Dir, Link, To))),

  % Evaluate
%catch(
  call(Eval),
%_, (gtrace, call(Nav, From, Dir, Link, To))),
  
  process_messages,
  
  fail.


process_messages:-
  thread_get_message(get_lifetime(Caller)), !,
  thread_self(Me),
  dh_navigation:number_of_steps(Lifetime),
  thread_send_message(Caller, lifetime(Me,Lifetime)).
process_messages.



% Messages

:- multifile(prolog:message//1).

print_steps(Steps):-
  print_message(informational, steps_taken(Steps)).

prolog:message(steps_taken(Steps)) -->
  ['~D steps have been taken.'-[Steps]].

