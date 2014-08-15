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

Also provides generic support for agent definitions,
e.g. listing the currently loaded agent definitions.

### Assumptions

We assume that every agent is represented by and implemented with
a Linux thread. These threads can be recognized (and enumerated)
by agent_thread/1 in [dh_population].

We also assume that the thread alias of an agent
is the same as the name of the agent's RDF graph,
which is used for storing beliefs the agent has.

--

@author Wouter Beek
@author Baudouin Duthoit
@version 2014/02, 2014/04, 2014/06-2014/08
*/

:- use_module(library(lists)).
:- use_module(library(predicate_options)). % Declarations.

:- use_module(plRdf(rdf_random)).

:- use_module(dh_core(dh_cycle)).
:- use_module(dh_core(dh_population)).

:- predicate_options(create_agent/4, 4, [
     pass_to(dh_cycle/3, 3)
   ]).



%! agent_self_graph(-Graph:atom) is semidet.
% Returns the name of this agent's RDF graph.
%
% With the phrase "this agent" we mean the agent that is
% implemented by the thread from within which this predicate is called.
%
% Silently fails if the current thread is not an agent.

agent_self_graph(Graph):-
  thread_self(Me),
  
  % Make sure the current thread denotes an agent.
  agent_thread(Me),
  
  % We use the assumption that the alias of an agent
  % is also the name of the RDF graph of an agent.
  thread_property(Me, alias(Graph)).


%! create_agent(+Kind:atom, +Initialization:compound) is det.
% Kind
% ====
% 
% The kind of agent that is created.
% This is the name of an agent definition as specified
% by the dynamic and multifile statements dh:agent_definition/2.
% An agent definition specifies the various predicates
% that define the agent's behavior.
%
%
% Initialization
% ==============
%
% An agent can be initialized in two ways:
% (1) locally and (2) globally/remotely.
%
% Local initialization
% --------------------
%
% The agent is initialized with respect to a locally loaded graph.
%
% The value of initialization is `graph(Graph)`,
% where `Graph` is the atomic name of a currently loaded RDF graph.
%
% Global/remote initialization
% ----------------------------
%
% The agent is initialized in the LOD cloud,
% which is a global data space consisting of all online servers
% that disseminate Linked Open Data (LOD).
%
% The value of initialization is `rdf(S,P,O)`, where `S`, `P`, and `O`
% are the subject-, predicate-, and object-terms that done
% an RDF statement (sometimes called a "triple").
% The agent starts at the dereference of this initial triple.
%
% If `S`, `P`, and `O` are uninstantiated, then a random triple
% will be taken from the DBpedia SPARQL endpoint,
% and that randomly chosen triple will be the initial triple for the agent.

% An agent definition that does not specific a special exit predicate.
create_agent(Kind, Initialization):-
  dh:agent_definition(Kind, [NavDef,ActDef,ComDef,EvalDef]), !,
  maplist(
    pred_from_predspec,
    [NavDef,ActDef,ComDef,EvalDef],
    [NavPred,ActPred,ComPred,EvalPred]
  ),
  create_agent(
    [NavPred,ActPred,ComPred,EvalPred],
    default_exit(Initialization),
    Initialization
  ).
% An agent definition that specifies a special exit predicate.
create_agent(Kind, Initialization):-
  dh:agent_definition(Kind, [NavDef,ActDef,ComDef,EvalDef,ExitDef]), !,
  maplist(
    pred_from_predspec,
    [NavDef,ActDef,ComDef,EvalDef,ExitDef],
    [NavPred,ActPred,ComPred,EvalPred,ExitPred1]
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
  dh_cycle:reset_number_of_cycles.


%! number_of_agents(-NumberOfAgents:nonneg) is det.

number_of_agents(N):-
  flag(number_of_agents, N, N).



% Helpers

% pred_from_predspec(+PredicateSpec, -Predicate:atom) is det.
% Returns the predicate name that occurs
% in the given predicate specification.
%
% A *predicate specification* is either a predicate name
% or a pair of a predicate name and a documentation string.
%
% Agent definitions consist of predicate specifications.
%
% @see Since a predicate specification may have no documentation string,
% we cannot use pairs_keys/2 from library [pairs] here.

pred_from_predspec(Pred-_, Pred):- !.
pred_from_predspec(Pred, Pred).

