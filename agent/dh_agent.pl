:- module(
  dh_agent,
  [
    create_agent/2, % +Agent:atom
                    % +Initialization:or([atom,compound])
    create_agents/3, % +NumberOfAgents:positive_integer
                     % +Agent:atom
                     % +Initialization:or([atom,compound])
    list_agents/0,
    number_of_agents/1, % -NumberOfAgents:nonneg
    rebirth/1 % +Type:atom
  ]
).

/** <module> DataHives agent

Create and kill agents in DataHives.

Also contains generic support for agent definitions,
e.g. listing the currently loaded agent definitions.

@author Wouter Beek
@author Baudouin Duthoit
@version 2014/02, 2014/04, 2014/06-2014/07
*/

:- use_module(library(predicate_options)).

:- use_module(generics(flag_ext)).

:- use_module(plRdf(rdf_random)).

:- use_module(plSparql(sparql_random)).

:- use_module(dh_core(dh_cycle)).

:- predicate_options(create_agent/4, 4, [
     pass_to(dh_cycle/3, 3)
   ]).



%! create_agent(+Agent:atom, +Initialization:or([atom,compound])) is det.

create_agent(Agent, Init):-
  dh:agent_definition(Agent, [Nav,Act,Com,Eva]), !,
  create_agent([Nav,Act,Com,Eva], default_exit, Init).
create_agent(Agent, Init):-
  dh:agent_definition(Agent, [Nav,Act,Com,Eval,Exit]), !,
  create_agent([Nav,Act,Com,Eval], Exit, Init).

%! create_agent(
%!   +Predicates:list(atom),
%!   :Exit,
%!   +Initialization:or([atom,compound])
%! ) is det.

% Initialize by graph.
create_agent(Preds, Exit, graph(Graph)):- !,
  rdf_random_triple(S, P, O, Graph),
  create_agent(Preds, Exit, rdf(S,P,O), [graph(Graph)]).
% Initialize by triple.
create_agent(Preds, Exit, rdf(S,P,O)):-
  create_agent(Preds, Exit, rdf(S,P,O), []).

%! create_agent(
%!   +Predicates:list(atom),
%!   :Exit,
%!   +InitialTriple:compound,
%!   +Options:list(nvpair)
%! ) is det.

create_agent(Preds, Exit, InitTriple, Options):-
  flag(agent, Id, Id + 1),
  format(atom(Alias), 'agent_~d', [Id]),
  thread_create(
    dh_cycle(Preds, InitTriple, Options),
    _,
    [alias(Alias),detached(true),at_exit(Exit)]
  ).


%! create_agents(
%!   +NumberOfAgents:positive_integer,
%!   +Agent:atom,
%!   +Initialization:or([atom,compound])
%! ) is det.

create_agents(N, Agent, Init):-
  forall(
    between(1, N, _),
    create_agent(Agent, Init)
  ).


%! list_agents is det.
% Lists all currently loaded agent definitions on current output.

list_agents:-
  forall(
    dh:agent_definition(Name, Definition),
    print_agent_definition(Name, Definition)
  ).

print_agent_definition(Name, Definition):-
  format('AGENT DEFINITION: ~a\n', [Name]),
  print_behaviors(
    ['NAVIGATE','ACT','COMMUNICATE','EVALUATE','REPRODUCE'],
    Definition
  ),
  format('\n').

print_behaviors(_, []).
print_behaviors([Prefix|T1], [Behavior|T2]):-
  format('  * ~a: ~17+~w\n', [Prefix,Behavior]),
  print_behaviors(T1, T2).


%! number_of_agents(-NumberOfAgents:nonneg) is det.

number_of_agents(N):-
  flag(agents, N, N).


%! default_exit is det.

default_exit:-
  retractall(backtrack(_)),
  retractall(deductions(_)),
  reset_thread_flag(number_of_cycles).

rebirth(Type):-
  sparql_random_triple(dbpedia, Resource),
  create_agent(Type, Resource).

