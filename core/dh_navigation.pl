:- module(
  dh_navigation,
  [
    backtrack/4,
    dh_navigate/5, % :Navigation,
                   % +From:or([bnode,iri,literal]),
                   % -Direction:oneof([backward,forward]),
                   % -Link:iri,
                   % -To:or([bnode,iri,literal])
    dh_navigation_init/1 % +InitialLocation:or([bnode,iri,literal])
  ]
).

/** <module> DataHives navigation

Navigation predicates for agents in DataHives.

@author Wouter Beek
@version 2014/02-2014/05
*/

:- use_module(library(semweb/rdf_db)). % Declarations.

%! backtrack(
%!   ?From:or([bnode,iri,literal]),
%!   ?Direction:oneof([backward,forward]),
%!   ?Link:iri,
%!   ?To:or([bnode,iri,literal])
%! ) is det.

:- thread_local(backtrack/4).

:- meta_predicate(dh_navigate(2,+,-,-,-)).
:- meta_predicate(dh_step(2,+,-,-,-)).

:- rdf_meta(dh_step(:,r,-,-,-)).

%! dh_current_location(
%!   +ThreadId:atom,
%!   -CurrentLocation:or([bnode,iri,literal])
%! ) is det.
% @tbd This requires thread communication.

dh_current_location(ThreadId, CurrentLocation):-
  thread_signal(ThreadId, backtrack(_,_,_,CurrentLocation)).


%! dh_navigate(
%!   :Navigation,
%!   +From:or([bnode,iri,literal]),
%!   -Direction:oneof([backward,forward]),
%!   -Link:iri,
%!   -To:or([bnode,iri,literal])
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

dh_navigate(Nav, From, Dir, Link, To):-
  backtrack(_, _, _, From),
  (
    dh_step(Nav, From, Dir, Link, To)
  ->
    retract(backtrack(_, _, _, _)),
    assert(backtrack(From, Dir, Link, To))
  ;
    retract(backtrack(To, Dir0, Link, From)),
    dir_inv(Dir0, Dir),
    assert(backtrack(From, Dir, Link, To))
  ).

dir_inv(backward, forward).
dir_inv(forward, backward).


dh_navigation_init(InitFrom):-
  assert(
    backtrack(
      InitFrom,
      forward,
      'http://www.w3.org/2002/07/owl#sameAs',
      InitFrom
    )
  ).


%! dh_step(
%!   :Navigation,
%!   +From:or([bnode,iri,literal]),
%!   -Direction:oneof([backward,forward]),
%!   -Link:iri,
%!   -To:or([bnode,iri,literal])
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

dh_step(Nav, From, Direction, Link, To):-
  % The nativation goal produces a single proposition,
  % where location `From` appears as either the subject or the object term.
  call(Nav, From, Proposition),

  % Find the direction of movement,
  % prefering forward movement in the case of symmetric links.
  (
    Proposition = [From,Link,To]
  ->
    Direction = forward
  ;
    Proposition = [To,Link,From]
  ->
    Direction = backward
  ).

