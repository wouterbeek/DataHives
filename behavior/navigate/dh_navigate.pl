:- module(
  dh_navigate,
  [
    backtrack/1, % ?DirectedTriple:compound
    dh_navigate/3, % :Navigation,
                   % ?DirectedTriple:compound
                   % +Options:list(nvpair)
    number_of_steps/1 % -NumberOfSteps:nonneg
  ]
).

/** <module> DataHives navigation

Navigation predicates for agents in DataHives.

@author Wouter Beek
@version 2014/02-2014/06
*/

:- use_module(library(semweb/rdf_db)). % Declarations.

:- use_module(generics(flag_ext)).

:- use_module(dh_core(dh_generic)).

%! backtrack(?DirectedTriple:compound) is det.

:- thread_local(backtrack/1).

:- meta_predicate(dh_navigate(3,?,+)).
:- meta_predicate(dh_step(3,?,-)).



%! dh_navigate(
%!   :Navigation,
%!   ?DirectedTriple:compound,
%!   +Options:list(nvpair)
%! ) is det.
% Walk from one location (called `From`) to a location (called `To`),
% via `Link`. `From` and `To` may be the same. When stuck,
% use some form of backtracking to avoid getting stuck too often.
%
% ### Direction
%
% Depending on the `Navigation` goal,
% walking may be eiter directional or bidirectional.
% The direction used for a specific step is indicated by `Direction`.
%
% ### Backtrack when stuck
%
% If it is not possible to take a step,
% then this predicate tries to backtrack to an earlier location
% and see whether navigation can progress from there.
%
% On the Semantic Web it is very easy to get stuck,
% e.g. no blank nodes, no literals, and only some IRIs are dereferencable.
%
% @tbd Backtracking is now very primitive.

dh_navigate(Nav, dir(From,Dir,Link,To), Options):-
  backtrack(dir(_,_,_,From)),
  (
    dh_step(Nav, dir(From,Dir,Link,To), Options)
  ->
    % Normal navigation is possible: there is no need to use
    % the backtrack option. Update the backtrack option for future use.
    retract(backtrack(_)),
    assert(backtrack(dir(From,Dir,Link,To)))
  ;
    % Use the backtrack assertion to navigate.
    retract(backtrack(dir(To,InvDir,Link,From))),
    invert_direction(InvDir, Dir),
    assert(backtrack(dir(From,Dir,Link,To)))
  ),
  thread_flag(number_of_steps, N, N + 1).


%! dh_step(
%!   :Navigation,
%!   +DirectedTriple:compound,
%!   +Options:list(nvpair)
%! ) is semidet.
% Take a single step from one location (called `From`)
% to a location (called `To`), via `Link`.
% `From` and `To` may be the same.
%
% We do not worry about getting stuck here.
% If no step can be found this predicate silently fails.
%
% ### Direction
%
% The direction parameter is instantiated
% in order to indicate whether the walk is forward or backward directed.
%
% Notice that we prefer forward motion in case both directions
% are possible.
% As a result of this it is impossible
% to walk *backwards* accross a symmetric link,
% e.g. from =|rdfs:Class|= to itself, via =|rdf:type|=.

dh_step(Nav, dir(From,Direction,Link,To), Options):-
  % The nativation goal produces a single proposition,
  % where location `From` appears as either the subject or the object term.
  call(Nav, From, Triple, Options),

  % Find the direction of movement,
  % prefering forward movement in the case of symmetric links.
  (
    Triple = rdf(From,Link,To)
  ->
    Direction = forward
  ;
    Triple = rdf(To,Link,From)
  ->
    Direction = backward
  ).


%! number_of_steps(-NumberOfSteps:nonneg) is det.

number_of_steps(N):-
  thread_flag(number_of_steps, N, N), !.
number_of_steps(0).

