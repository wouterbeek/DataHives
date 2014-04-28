:- module(
  dh_agent,
  [
    create_agent/4, % :Navigate
                    % :Act
                    % :Communicate
                    % +InitialLocation:iri
    create_agents/5 % :Navigate
                    % :Act
                    % :Communicate
                    % +InitialLocation:iri
                    % +NumberOfAgents:positive_integer
  ]
).

/** <module> DataHives agent

Create and kill agents in DataHives.

@author Wouter Beek
@version 2014/02, 2014/04
*/

:-  use_module(dh_core(dh_cycle)).

:- meta_predicate(create_agent(4,4,4,+)).
:- meta_predicate(create_agents(4,4,4,+,+)).



%! create_agent(:Navigate, :Act, :Communicate, +InitialLocation:url) .

create_agent(Nav, Act, Com, Init):-
  flag(agent, Id, Id + 1),
  format(atom(Alias), 'agent_~d', [Id]),
  thread_create(
    dh_cycle(Nav, Act, Com, Init),
    _,
    [alias(Alias)]
  ).


%! create_agents(
%!   :Navigate,
%!   :Act,
%!   :Communicate,
%!   +InitialLocation:url,
%!   +N:positive_integer
%! ) .

create_agents(Nav, Act, Com, Init, N):-
  forall(
    between(1, N, _),
    create_agent(Nav, Act, Com, Init)
  ).

