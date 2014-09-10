:- module(
  dh_agent_create,
  [
    dh_agent_create/2, % +AgentDefinition:url
                       % +Initialization:compound
    dh_agents_create/3, % +NumberOfAgents:positive_integer
                        % +AgentDefinition:url
                        % +Initialization:compound
    dh_agent_delete/0,
    dh_agent_delete/1 % +Agent:url
  ]
).

/** <module> DataHives Agent: Create

Predicate for creating agents in DataHives.

Also provides generic support for agent definitions,
e.g. listing the currently loaded agent definitions.

### Assumptions

We assume that every agent is represented by and implemented with
a Linux thread. These threads can be recognized (and enumerated)
by `dh_agent_property(Agent, thread, Thread)` in [dh_agent_property].

We also assume that the thread alias of an agent
is the same as the name of the agent's RDF graph,
which is used for storing beliefs the agent has.

--

@author Wouter Beek
@author Baudouin Duthoit
@version 2014/02, 2014/04, 2014/06-2014/09
*/

:- use_module(library(http/http_path)).
:- use_module(library(lists)).
:- use_module(library(predicate_options)). % Declarations.

:- use_module(plRdf(rdf_build)).
:- use_module(plRdf(rdf_random)).
:- use_module(plRdf(rdfs_label_ext)).

:- use_module(dh_agent_definition(dh_agent_definition)).
:- use_module(dh_core(dh_cycle)).
:- use_module(dh_core(dh_messages)).

:- predicate_options(dh_agent_create/4, 4, [
     pass_to(dh_cycle/3, 3)
   ]).



%! dh_agent_create(+AgentDefinition:url, +Initialization:compound) is det.
% AgentDefinition
% ===============
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
dh_agent_create(AgentDefinition, Initialization):-
  dh_agent_definition(
    AgentDefinition,
    [NavDef,ActDef,ComDef,EvalDef]
  ), !,
  maplist(
    pred_from_predspec,
    [NavDef,ActDef,ComDef,EvalDef],
    [NavPred,ActPred,ComPred,EvalPred]
  ),
  dh_agent_create(
    AgentDefinition,
    [NavPred,ActPred,ComPred,EvalPred],
    dh_agent_delete,
    Initialization
  ).
% An agent definition that specifies a special exit predicate.
dh_agent_create(AgentDefinition, Initialization):-
  dh_agent_definition(
    AgentDefinition,
    [NavDef,ActDef,ComDef,EvalDef,ExitDef]
  ), !,
  maplist(
    pred_from_predspec,
    [NavDef,ActDef,ComDef,EvalDef,ExitDef],
    [NavPred,ActPred,ComPred,EvalPred,ExitPred1]
  ),

  % Add the initialization argument to the exit predicate.
  ExitPred1 =.. [ExitPred0|ExitArgs1],
  append(ExitArgs1, [Initialization], ExitArgs2),
  ExitPred2 =.. [ExitPred0|ExitArgs2],

  dh_agent_create(
    AgentDefinition,
    [NavPred,ActPred,ComPred,EvalPred],
    ExitPred2,
    Initialization
  ).


%! dh_agent_create(
%!   +AgentDefinition:url,
%!   +Predicates:list(atom),
%!   :Exit,
%!   +Initialization:or([atom,compound])
%! ) is det.
% @arg Initialization Either a graph, denoted by =|graph(+atom)|=
%      or a triple, denoted by =|rdf(+subject,+predicate,+object)|=.

% Initialize by graph.
dh_agent_create(AgentDefinition, Preds, Exit, graph(Graph)):- !,
  rdf_random_triple(S, P, O, Graph),
  dh_agent_create(AgentDefinition, Preds, Exit, rdf(S,P,O), [graph(Graph)]).
% Initialize by triple.
dh_agent_create(AgentDefinition, Preds, Exit, rdf(S,P,O)):-
  dh_agent_create(AgentDefinition, Preds, Exit, rdf(S,P,O), []).

%! dh_agent_create(
%!   +AgentDefinition:url,
%!   +Predicates:list(atom),
%!   :Exit,
%!   +InitialTriple:compound,
%!   +Options:list(nvpair)
%! ) is det.

dh_agent_create(AgentDefinition, Preds, ExitPred, InitialTriple, Options):-
  % Construct the agent resource.
  flag(number_of_agents, Id, Id + 1),
  http_absolute_uri(dh_agent(Id), Agent),
  rdf_assert_instance(Agent, AgentDefinition, dh),
  
  % Construct the agent thread alias / RDF graph name.
  format(atom(Alias), 'agent_~d', [Id]),
  rdfs_assert_label(Agent, Alias, dh),

  % Start the thread.
  thread_create(
    dh_cycle(Preds, InitialTriple, Options),
    _,
    [alias(Alias),at_exit(ExitPred),detached(true)]
  ).


%! dh_agents_create(
%!   +NumberOfAgents:positive_integer,
%!   +AgentDefinition:url,
%!   +Initialization:or([atom,compound])
%! ) is det.

dh_agents_create(N, AgentDefinition, Initialization):-
  forall(
    between(1, N, _),
    dh_agent_create(AgentDefinition, Initialization)
  ).


%! dh_agent_delete is det.

dh_agent_delete:-
  retractall(backtrack(_)),
  thread_exit(true).


%! dh_agent_delete(+Agent:url) is det.

dh_agent_delete(Agent):-
  dh_agent_command(Agent, dh_agent_delete).



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

