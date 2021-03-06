:- module(
  dh_cycle,
  [
    dh_cycle/3, % +Predicates:list(atom)
                % +InitialTriple:compound
                % +Options:list(nvpair)
    cycles/1 % ?Cycles:nonneg
  ]
).

/** <module> DataHives cycle

The navigate-act-communicate cycle for agents in DataHives.

@author Wouter Beek
@version 2014/04-2014/09
*/

:- use_module(generics(flag_ext)).

:- use_module(plRdf(api/rdfs_build2)).

:- use_module(dh(agent/dh_agent)).
:- use_module(dh(beh/dh_beh)).
:- use_module(dh(beh/nav/dh_walk)).
:- use_module(dh(core/dh_generics)).
:- use_module(dh(core/dh_messages)).

%! creation(-DateTime:integer) is semidet.

:- thread_local(creation/1).

%! number_of_cycles(-NumberOfCycles:nonneg) is semidet.

:- thread_local(number_of_cycles/1).

:- meta_predicate(call_every_n_cycles(+,1)).



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
  set_backtrack(InitDirTriple),

  % Store the creation datetime.
  get_time(Creation),
  assert(creation(Creation)),

  % CHOICEPOINT.
  repeat,
  call_every_n_cycles(10, print_number_of_cycles),

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

  increment_number_of_cycles,

  fail.



% Agent property

% cycles(+Cycles:nonneg) is semidet.
% cycles(-Cycles:nonneg) is det.

cycles(Cycles):-
  number_of_cycles(Cycles), !.
cycles(0).



% Helpers

call_every_n_cycles(N, Goal):-
  cycles(M),
  M > 0,
  M mod N =:= 0, !,
  call(Goal, M).
call_every_n_cycles(_, _).


%! increment_number_of_cycles is det.

increment_number_of_cycles:-
  increment_number_of_cycles(1).

%! increment_number_of_cycles(+Increment:integer) is det.

increment_number_of_cycles(N2):-
  retract(number_of_cycles(N1)), !,
  N3 is N1 + N2,
  assert(number_of_cycles(N3)).
increment_number_of_cycles(N):-
  assert(number_of_cycles(N)).



% Messages

:- multifile(prolog:message//1).

print_number_of_cycles(N):-
  sleep(1), %DEB
  print_message(informational, dh_agent_property(cycles,N)),
  true.

prolog:message(dh_agent_property(cycles,N)) -->
  ['~D cycles.'-[N]].

