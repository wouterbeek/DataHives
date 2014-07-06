:- module(
  dh_agent,
  [
    agent_self_graph/1, % -Graph:atom
    create_agent/2, % +Agent:atom
                    % +Initialization:compound
    create_agents/3, % +NumberOfAgents:positive_integer
                     % +Agent:atom
                     % +Initialization:compound
    default_exit/1, % +Initialization:compound
    number_of_agents/1 % -NumberOfAgents:nonneg
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

:- use_module(library(lists)).
:- use_module(library(predicate_options)). % Declarations.

:- use_module(generics(flag_ext)).

:- use_module(plRdf(rdf_random)).

:- use_module(dh_core(dh_cycle)).

:- predicate_options(create_agent/4, 4, [
     pass_to(dh_cycle/3, 3)
   ]).



%! agent_self_graph(-Graph:atom) is semidet.
% Returns the name of this agent thread's RDF graph.
%
% Silently fails if the current thread is not an agent.

agent_self_graph(Graph):-
  thread_self(Me),
  thread_property(Me, alias(Graph)).


%! create_agent(+Agent:atom, +Initialization:compound) is det.

create_agent(Agent, Initialization):-
  dh:agent_definition(Agent, [NavDef,ActDef,ComDef,EvalDef]), !,
  maplist(
    split_pred_doc,
    [NavDef,ActDef,ComDef,EvalDef],
    [NavPred,ActPred,ComPred,EvalPred],
    _
  ),
  create_agent(
    [NavPred,ActPred,ComPred,EvalPred],
    default_exit(Initialization),
    Initialization
  ).
create_agent(Agent, Initialization):-
  dh:agent_definition(Agent, [NavDef,ActDef,ComDef,EvalDef,ExitDef]), !,
  maplist(
    split_pred_doc,
    [NavDef,ActDef,ComDef,EvalDef,ExitDef],
    [NavPred,ActPred,ComPred,EvalPred,ExitPred1],
    _
  ),

  % Add the initialization argument to the exit predicate.
  ExitPred1 =.. [ExitPred0|ExitArgs1],
  append(ExitArgs1, [Initialization], ExitArgs2),
  ExitPred2 =.. [ExitPred0|ExitArgs2],

  create_agent([NavPred,ActPred,ComPred,EvalPred], ExitPred2, Initialization).

%! create_agent(
%!   +Predicates:list(atom),
%!   :Exit,
%!   +Initialization:or([atom,compound])
%! ) is det.
% @arg Initialization Either a graph, denoted by =|graph(+atom)|=
%      or a triple, denoted by =|rdf(+subject,+predicate,+object)|=.

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

create_agent(Preds, ExitPred, InitialTriple, Options):-
  flag(number_of_agents, Id, Id + 1),
  format(atom(Alias), 'agent_~d', [Id]),
  thread_create(
    dh_cycle(Preds, InitialTriple, Options),
    _,
    [alias(Alias),at_exit(ExitPred),detached(true)]
  ).


%! create_agents(
%!   +NumberOfAgents:positive_integer,
%!   +Agent:atom,
%!   +Initialization:or([atom,compound])
%! ) is det.

create_agents(N, Agent, Initialization):-
  forall(
    between(1, N, _),
    create_agent(Agent, Initialization)
  ).


%! default_exit(+Initialization:compound) is det.

default_exit(_):-
  % @tbd Move to navigate: walk.
  retractall(backtrack(_)),
  % @tbd Move to act: decution.
  retractall(deductions(_)),
  reset_thread_flag(number_of_cycles).


%! number_of_agents(-NumberOfAgents:nonneg) is det.

number_of_agents(N):-
  flag(number_of_agents, N, N).



% Helpers

split_pred_doc(Pred-Doc, Pred, Doc):- !.
split_pred_doc(Pred, Pred, _).

