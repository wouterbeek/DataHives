:- module(
  dh_com,
  [
    no_communication/1, % ?DirectedTriple:compound
    forbide_path/1, % +From:or([bnode,iri,literal])
    spawn_foragers/3 % +InterestLevel:nonneg
                     % +NumberOfForagers:nonneg
                     % +DirectedTriple:compound
  ]
).

/** <module> DataHives communication

Communication predicates for agents in DataHives.

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/02, 2014/04-2014/05, 2014/07
*/

:- use_module(library(ansi_term)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).

:- use_module(dh_act(dh_search)).
:- use_module(dh_agent(dh_agent_create)).
:- use_module(dh_core(dh_generics)).
:- reexport(dh_com(dh_edge_weight)).



no_communication(dir(_,_,_,_)).


% In case of dead ends, we must tell the others about the non viability
% of the path. Unfortunately (or fortunately), this function seems not
% being used.

forbide_path(To):-
  single_alley(To,Alley),
  ansi_format([italic,fg(red)],'~w',['WrongPath\n']),
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


%! devalue(+Path:list(compound)) is det.
% Value of triple S-P-O decreased by one

devalue(Triples):-
  maplist(update_edge_count(-1), Triples).


%! spawn_foragers(
%!   +InterestLevel:nonneg,
%!   +NumberOfForagers:nonneg,
%!   +DirectedTriple:compound
%! ) is det.
% @tbd Specific to scout agent, relocate.

spawn_foragers(InterestLevel, NumberOfForagers, DirectedTriple):-
  update_edge_count(DirectedTriple), %DEB
  number_of_overall_search_results(NumberOfSearchResults),
  (
    NumberOfSearchResults >= InterestLevel
  ->
    directed_triple(DirectedTriple, Triple),
    http_link_to_id(
      dh_agent_definition,
      path_postfix(forager),
      AgentDefinition
    ),
    forall(
      between(1, NumberOfForagers, _),
      dh_agent_create(AgentDefinition, Triple)
    )
  ;
    true
  ).

