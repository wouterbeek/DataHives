:- module(
  dh_cycle,
  [
    dh_cycle/3, % +Predicates:list(atom)
                % +InitialTriple:compound
                % +Options:list(nvpair)
    number_of_cycles/1 % -Cycles:nonneg
  ]
).

/** <module> DataHives cycle

The navigate-act-communicate cycle for agents in DataHives.

@author Wouter Beek
@version 2014/04-2014/06
*/

:- use_module(library(option)).
:- use_module(library(predicate_options)).

:- use_module(generics(flag_ext)).

:- use_module(dh_com(dh_edge_weight)). % Agent components.
:- use_module(dh_core(dh_act)). % Agent components.
:- use_module(dh_core(dh_evaluate)). % Agent components.
:- use_module(dh_core(dh_generic)).
:- use_module(dh_core(dh_messages)).
:- use_module(dh_core(dh_navigate)). % Agent components.
:- use_module(dh_core(dh_communicate)). % Agent components.
:- use_module(dh_nav(dh_bee_fly)). % Agent components.
:- use_module(dh_nav(dh_random_lod_walk)). % Agent components.
:- use_module(dh_nav(dh_weighted_lod_walk)). % Agent components.



%! dh_cycle(
%!   +Predicates:list(atom),
%!   +InitialTriple:compound,
%!   +Options:list(nvpair)
%! ) .
% Implementation of the navigate-act-communicate cycle.
%
% When the walker visits a blank node or a literal,
% it is impossible to use the LOD walker
% since literals and blank nodes give no information
% as to where on the Web we may find linked data.
%
% The `Backtrack` parameter is instatiated with the last traversal,
% which is the node that will be visited after a literal or blank node
% was visited.
%
% The same is true for any term that does not dereference,
% such as non-dereferencing URLs.
%
% We assue that the following predicates are provided (in this order):
%   * Navigate
%   * Act
%   * Communicate
%   * Evaluate

dh_cycle([Nav,Act,Com,Eval], InitTriple, Options):-
  % Initialize the backtrack fact.
  directed_triple(InitDirTriple, InitTriple),
  assert(backtrack(InitDirTriple)),

  reset_thread_flag(number_of_cycles),

  % CHOICEPOINT.
  repeat,

  call_every_n(number_of_cycles, 100, print_number_of_cycles),

  % Navigate.
  call(Nav, dir(From,Dir,Link,To), Options),

  % Act.
  call(Act, dir(From,Dir,Link,To)),

  % Communicate.
  call(Com, dir(From,Dir,Link,To)),

  % Evaluate
  call(Eval),

  % Process message queue.
  process_messages,

  fail.


number_of_cycles(N):-
  thread_flag(number_of_cycles, N, N), !.
number_of_cycles(0).



% Messages

:- multifile(prolog:message//1).

print_number_of_cycles(N):-
  gtrace, %DEB
  print_message(informational, number_of_cycles(N)).

prolog:message(number_of_cycles(N)) -->
  ['~D cycles.'-[N]].

