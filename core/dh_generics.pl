:- module(
  dh_generics,
  [
    directed_triple/2, % ?DirectedTriple:compound
                       % ?Triple:compound
    direction/2, % +DirectedTriple:compound
                 % +Direction:oneof([backward,forward])
    ensure_triple/2, % +DirectedTripleOrTriple:compound
                     % -Triple:compound
    forward_directed_triple/2, % +DirectedTriple:compound
                               % -ForwardDirectedTriple:compound
    invert_directed_triple/2, % +DirectedTriple:compound
                              % -InvertedDirectedTriple:compound
    invert_direction/2 % ?Direction:oneof([backward,forward]),
                       % ?InvertedDirection:oneof([backward,forward])
  ]
).

/** <module> DataHives generics

Generic predicates that are used in DataHives.

@author Wouter Beek
@version 2014/06, 2014/08-2014/09, 2014/11
*/

:- use_module(plRdf(management/rdf_load_any)).

:- initialization(init_xsdo).





%! directed_triple(+DirectedTriple:compound, +Triple:compound) is semidet.
%! directed_triple(+DirectedTriple:compound, -Triple:compound) is det.
%! directed_triple(-DirectedTriple:compound, +Triple:compound) is det.

directed_triple(DirTriple, Triple):-
  var(DirTriple), !,
  directed_triple0(DirTriple, Triple), !.
directed_triple(DirTriple, Triple):-
  directed_triple0(DirTriple, Triple).

% Order is significant here.
% Instantiation (-,+) should return a forward directed triple
% deterministically.
directed_triple0(dir(S,forward,P,O), rdf(S,P,O)).
directed_triple0(dir(O,backward,P,S), rdf(S,P,O)).


%! direction(+DirectedTriple:compound, +Direction) is semidet.
%! direction(+DirectedTriple:compound, -Direction) is det.

direction(dir(_,Direction,_,_), Direction).


%! ensure_triple(
%!   +DirectedTripleOrTriple:compound,
%!   -Triple:compound
%! ) is det.

ensure_triple(rdf(S,P,O), rdf(S,P,O)):- !.
ensure_triple(DirTriple, Triple):-
  directed_triple(DirTriple, Triple), !.


%! forward_directed_triple(
%!    +DirectedTriple:compound,
%!    -ForwardDirectedTriple:compound
%! ) is det.
% Ensures that a directed triple is pointing forward.

forward_directed_triple(
  dir(From,backward,Link,To),
  dir(To,forward,Link,From)
):- !.
  forward_directed_triple(DirTriple, DirTriple).


%! invert_directed_triple(
%!   +DirectedTriple:compound,
%!   -InvertedDirectedTriple:compound
%! ) is det.

invert_directed_triple(dir(From,Dir,Link,To), dir(To,InvDir,Link,From)):-
  invert_direction(Dir, InvDir).


%! invert_direction(
%!    +Direction:oneof([backward,forward]),
%!    +InvertedDirection:oneof([backward,forward])
%! ) is semidet.
%! invert_direction(
%!    +Direction:oneof([backward,forward]),
%!    -InvertedDirection:oneof([backward,forward])
%! ) is det.
%! invert_direction(
%!    -Direction:oneof([backward,forward]),
%!    +InvertedDirection:oneof([backward,forward])
%! ) is det.

invert_direction(backward, forward).
invert_direction(forward, backward).





% INITIALIZATION

% XSD ontology used in agent (local/global) properties.

init_xsdo:-
  absolute_file_name(xsdo, File, [file_type(turtle)]),
  rdf_load_any(File, [graph(xsdo)]).

