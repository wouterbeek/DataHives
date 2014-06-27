:- module(
  dh_agent,
  [
    create_agent/2, % +Agent:atom
                    % +Initialization:or([atom,compound])
    create_agents/3, % +NumberOfAgents:positive_integer
                     % +Agent:atom
                     % +Initialization:or([atom,compound])
    number_of_agents/1 % -NumberOfAgents:nonneg
  ]
).

/** <module> DataHives agent

Create and kill agents in DataHives.

@author Wouter Beek
@version 2014/02, 2014/04, 2014/06
*/

:- use_module(library(predicate_options)).

:- use_module(plRdf(rdf_random)).

:- use_module(dh_core(dh_cycle)).

:- predicate_options(create_agent/4, 4, [
     pass_to(dh_cycle/3, 3)
   ]).



%! create_agent(+Agent:atom, +Initialization:or([atom,compound])) is det.

create_agent(Agent, Init):-
  user:agent_definition(Agent, [Nav,Act,Com,Eva]), !,
  create_agent([Nav,Act,Com,Eva], true, Init).
create_agent(Agent, Init):-
  user:agent_definition(Agent, [Nav,Act,Com,Eval,Exit]), !,
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


%! number_of_agents(-NumberOfAgents:nonneg) is det.

number_of_agents(N):-
  flag(agents, N, N).

