:- module(
  dh_agent_property,
  [
    dh_agent_property/1, % ?Property:iri
    dh_agent_property/3 % ?Agent:iri
                        % ?Property:iri
                        % ?Value
  ]
).

/** <module> DataHives Agent: Property

Combined access to global and local agent properties.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(semweb/rdfs)).

:- use_module(dh(agent/dh_agent_property_global)).
:- use_module(dh(agent/dh_agent_property_local)).



%! dh_agent_property(+Property) is semidet.
%! dh_agent_property(-Property) is multi.

dh_agent_property(Property):-
  rdfs_subproperty_of(Property, dho:agentProperty),
  \+ ((
    rdfs_subproperty_of(Property0, Property),
    Property0 \== Property
  )).


%! dh_agent_property(+Agent:iri, +Property:iri, +Value) is semidet.
%! dh_agent_property(+Agent:iri, +Property:iri, -Value) is semidet.
%! dh_agent_property(+Agent:iri, -Property:iri, -Value) is multi.
%! dh_agent_property(-Agent:iri, -Property:iri, -Value) is nondet.

dh_agent_property(Agent, Property, Value):-
  dh_agent_property_global(Agent, Property, Value).
dh_agent_property(Agent, Property, Value):-
  dh_agent_property_local(Agent, Property, Value).
