:- module(
  dh_cycle,
  [
    dh_cycle/5, % :Navigate
                % :Act
                % :Communicate
                % :Evaluate
                % +InitialResource:iri
    number_of_cycles/1 % -Cycles:nonneg
  ]
).

/** <module> DataHives cycle

The navigate-act-communicate cycle for agents in DataHives.

@author Wouter Beek
@version 2014/04-2014/06
*/

:- use_module(generics(flag_ext)).

:- use_module(dh_core(dh_messages)).
:- use_module(dh_core(dh_navigate)).

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
  dh_navigate_init(InitFrom),
  init_call_every_n(number_of_cycles),

  % CHOICEPOINT.
  repeat,

  call_every_n(number_of_cycles, 100, print_number_of_cycles),

  % Navigate.
gtrace,
  call0(Nav, From, Dir, Link, To),

  % Act.
  call0(Act, From, Dir, Link, To),

  % Communicate.
  call0(Com, From, Dir, Link, To),

  % Evaluate
  call(Eval),

  % Process message queue.
  process_messages,

  fail.

:- meta_predicate(call0(:,?,?,?,?)).
call0(Goal, Q, X, Y, Z):-
  %fail, %DEB
  call(Goal, Q, X, Y, Z), !.
call0(Goal, Q, X, Y, Z):-
  catch(
    call(Goal, Q, X, Y, Z),
    _,
    (gtrace, call(Goal, Q, X, Y, Z))
  ).


number_of_cycles(N):-
  thread_flag(number_of_cycles, N, N), !.
number_of_cycles(0).



% Messages

:- multifile(prolog:message//1).

print_number_of_cycles(N):-
  print_message(informational, number_of_cycles(N)).

prolog:message(number_of_cycles(N)) -->
  ['~D cycles.'-[N]].

