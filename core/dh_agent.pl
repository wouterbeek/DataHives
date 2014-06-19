:- module(
  dh_agent,
  [
    create_agent/6, % :Navigate
                    % :Act
                    % :Communicate
                    % :Evaluate
                    % :Exit
                    % +InitialLocation:or([atom,iri])
    create_agents/7 % :Navigate
                    % :Act
                    % :Communicate
                    % :Evaluate
                    % :Exit
                    % +InitialLocation:or([atom,iri])
                    % +NumberOfAgents:positive_integer
  ]
).

/** <module> DataHives agent

Create and kill agents in DataHives.

@author Wouter Beek
@version 2014/02, 2014/04, 2014/06
*/

:- use_module(library(aggregate)).
:- use_module(library(random)).
:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf_term(rdf_term)).

:- use_module(dh_core(dh_cycle)).

:- meta_predicate(create_agent(4,4,4,0,:,+)).
:- meta_predicate(create_agents(4,4,4,0,:,+,+)).



%! create_agent(
%!   :Navigate,
%!   :Act,
%!   :Communicate,
%!   :Evaluate,
%!   :Exit
%!   +InitialLocation:url
%! ) .

% Initialize by graph.
create_agent(Nav, Act, Com, Eval, Exit, Graph):-
  rdf_graph(Graph), !,

  % Take a random term out of the given graph.
  aggregate_all(
    set(Term),
    rdf_term(Term, Graph),
    Terms
  ),
  random_member(Term, Terms),
% Initialize the agent with the found term.
  create_agent(Nav, Act, Com, Eval, Exit, Term).

% Initialize by term.
create_agent(Nav, Act, Com, Eval, Exit, Term):-
  flag(agent, Id, Id + 1),
  format(atom(Alias), 'agent_~d', [Id]),
  thread_create(
    dh_cycle(Nav, Act, Com, Eval, Term),
    _,
    [alias(Alias),detached(true),at_exit(Exit)]
  ).


%! create_agents(
%!   :Navigate,
%!   :Act,
%!   :Communicate,
%!   :Evaluate,
%!   +InitialLocation:url,
%!   +NumberOfAgents:positive_integer
%! ) .

create_agents(Nav, Act, Com, Eval, Exit, Init, N):-
  forall(
    between(1, N, _),
    create_agent(Nav, Act, Com, Eval, Exit, Init)
  ).

