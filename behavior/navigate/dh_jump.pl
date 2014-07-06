:- module(
  dh_jump,
  [
    dh_jump/3 % :Navigate
              % -DirectedTriple:compound
              % +Options:list(nvpair)
  ]
).

/** <module> DataHives jump

Generic architecture around jumping strategies for navigation.

We define jumping as navigating from a previous location
to a location that need not be structurally related to the former.

@author Wouter Beek
@version 2014/07
*/

:- use_module(library(predicate_options)). % Declarations.

:- use_module(generics(flag_ext)).

:- meta_predicate(dh_jump(2,-,+)).

:- predicate_options(dh_jump/3, 3, [
     direction(+oneof([backward,both,forward]))
   ]).



%! dh_jump(:Navigate, -DirectedTriple:compound, +Options:list(nvpair)) is det.
% Performs one jump, based on the given navigation strategy.
%
% Since in jumping the new triple does not relate to the old one,
% there is no such thing as a forward and backward direction.
% For ease of implementation we use `forward` for all "directed" triples
% while jumping.
%
% The following options are implemented:
%   * =|direction(+oneof([backward,both,forward]))|=

dh_jump(Nav, dir(From,forward,Link,To), Options):-
  call(Nav, rdf(From,Link,To), Options),
  thread_flag(number_of_steps, N, N + 1).

