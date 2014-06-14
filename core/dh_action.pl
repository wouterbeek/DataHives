:- module(
  dh_action,
  [
    default_action/4, % +From:or([bnode,iri,literal])
                      % -Direction:oneof([backward,forward])
                      % -Link:iri
                      % -To:or([bnode,iri,literal])
    deductive_action/4, % +From:or([bnode,iri,literal])
                        % -Direction:oneof([backward,forward])
                        % -Link:iri
                        % -To:or([bnode,iri,literal])
    kill_agent/0,
    forbide_path/1 % +From:or([bnode,iri,literal])
  ]
).

/** <module> DataHives actions

Action predicates for agents in DataHives.

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/02, 2014/04, 2014/06
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).

:- use_module(plRdf(rdf_name)).
:- use_module(plRdf_ent(rdf_entailment_patterns)).
:- use_module(plRdf_term(rdf_term)).

:- use_module(dh_core(dh_communication)).
:- use_module(dh_core(dh_navigation)).
:- use_module(dh_test(dh_test)).

%! fitness(?Deductions:nonneg, ?Lifetime:nonneg) is nondet.

:- thread_local(fitness/2).



default_action(From, Dir, Link, To):-
  dir_trans(Dir, Orient),
  dcg_with_output_to(atom(Arrow), arrow(Orient, 4)),
  dcg_with_output_to(atom(Triple), rdf_triple_name(From, Link, To)),
  debug(dh, '~w\t~w', [Arrow,Triple]).


deductive_action(From, backward, Link, To):- !,
  deductive_action(To, forward, Link, From).
deductive_action(From, forward, Link, To):-
  thread_self(Me),
  thread_property(Me, alias(MyName)),
  forall(
    (
      rdf_entailment_pattern(rdf(From,Link,To), rdf(U,V,W))
    ;
      rdf_entailment_pattern(rdf(From,Link,To), rdf(X,Y,Z), rdf(U,V,W)),
      rdf(X, Y, Z)
    ),
    (
      rdf_assert(U, V, W, MyName),
      increment_fitness
    )
  ),
  increment_lifetime,
  print_message(informational, fitness(MyName)).


increment_fitness:-
  (
    retract(fitness(Deductions1, Lifetime1)), !
  ;
    Deductions1 = 0,
    Lifetime1 = 0
  ),
  maplist(succ, [Deductions1,Lifetime1], [Deductions2,Lifetime2]),
  assert(fitness(Deductions2, Lifetime2)).


increment_lifetime:-
  (
    retract(fitness(Deductions, Lifetime1)), !
  ;
    Deductions = 0,
    Lifetime1 = 0
  ),
  succ(Lifetime1, Lifetime2),
  assert(fitness(Deductions, Lifetime2)).


kill_agent:-
  dh_supervised_test(_),
  write("Reborn"),
  thread_exit(_).

forbide_path(From):-
  single_alley(From,Alley),
  devalue(Alley).

single_alley(Next, [[Prev,Link,Next]|Alley]):-
  single_step(Prev, Link, Next),
  single_alley(Prev, Alley).
single_alley(_, []).

single_step(Prev, Link, Next):-
  rdf(Prev, Link, Next),
  \+ ((
    rdf(Prev, Link0, Next0),
    Link0 \== Link,
    Next0 \== Next
  )).

% Value of triple S-P-O decreased by one
%
devalue([]):-!.
devalue([H|T]):-
  devalueTriple(H),
  devalue(T).
devalueTriple([S,P,O]):-
  edge_count(S,P,O,Count),
  succ(NextValue,Count),  % Should we use -2 instead of -1 ?
  edge_count(S,P,O,NextValue).

dir_trans(backward, left).
dir_trans(forward, right).


% Messages

:- multifile(prolog:message//1).

prolog:message(fitness(MyName)) -->
  {
    fitness(Deductions, Lifetime),
    Fitness is Deductions / Lifetime
  },
  ['[~w] ~f (~D deductions; ~D steps)'-[MyName,Fitness,Deductions,Lifetime]].

