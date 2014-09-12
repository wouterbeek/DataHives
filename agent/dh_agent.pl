:- module(
  dh_agent,
  [
    dh_agent/1, % ?Agent:url
    dh_agent_rest/2 % +Request:list(nvpair)
                    % +HtmlStyle
  ]
).

/** <module> DataHives Agent

Interface to agents in DataHives.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(lambda)).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(request_ext)).

:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(dcg_generic)).

:- use_module(plHtml(html)).
:- use_module(plHtml(html_pl_term)).
:- use_module(plHtml(html_table)).

:- use_module(dh_agent(dh_agent_create)).
:- use_module(dh_agent(dh_agent_property)).
:- use_module(dh_core(dh_generics), []). % RDF namespace.
:- use_module(dh_core(dh_population)).
:- use_module(dh_web(dh_web_generics)).



%! dh_agent(+Agent:url) is semidet.
%! dh_agent(-Agent:url) is nondet.

dh_agent(Agent):-
  rdfs_individual_of(Agent, dho:'Agent').


% GET html *
dh_agent_rest(Request, HtmlStyle):-
  cors_enable,
  request_filter(Request, get, _/html, Root),
  http_absolute_uri(dh_agent(.), Root), !,
gtrace,
  aggregate_all(
    set(Property),
    rdfs_subproperty_of(Property, dho:agentProperty),
    Properties
  ),
  findall(
    [Label-Agent|Row],
    (
      dh_agent(Agent),
      rdfs_label(Agent, Label),
      maplist(
        \Property^Value^(dh:dh_agent_property(Agent, Property, Value)),
        Properties,
        Row
      )
    ),
    Rows
  ),
  dh_population_property(dho:size, Size),
  maplist(
    \Property^Header^dcg_phrase(capitalize, Property, Header),
    Properties,
    HeaderRow
  ),
  reply_html_page(
    HtmlStyle,
    \dh_agent_head(['Overview']),
    \dh_body(
      \html_table(
        html([
          'Overview of the ',
          \html_pl_term(dh, Size),
          ' currently running agents in DataHives.'
        ]),
        [HeaderRow|Rows],
        [header_row(true),index(true)]
      )
    )
  ).
% GET html PATH
% Returns 404 (Not Found) is the agent does not exist on the server.
dh_agent_rest(Request, HtmlStyle):-
  cors_enable,
  request_filter(Request, get, _/html, Agent), !,

  % Reply with a failure code if the agent does not exist.
  http_exists(Request, dh_agent(Agent)),

  findall(
    [Name,Value],
    dh:dh_agent_property(Agent, Name, Value),
    Rows
  ),
  rdfs_label(Agent, Label),
  reply_html_page(
    HtmlStyle,
    \dh_agent_head([Label]),
    \dh_body(
      \html_table(
        html(['Overview of agent ', \html_link(Agent-Label), '.']),
        Rows,
        [header_row(false),index(false)]
      )
    )
  ).
% GET json *
dh_agent_rest(Request, _):-
  cors_enable,
  request_filter(Request, get, _/json, Root),
  http_absolute_uri(dh_agent(.), Root), !,
  findall(
    Dict,
    dh_agent_properties_json(_, Dict),
    Dicts
  ),
  reply_json_dict(json{agentProperties:Dicts}).
% GET json PATH
dh_agent_rest(Request, _):-
  cors_enable,
  request_filter(Request, get, _/json, Agent), !,
  http_exists(Request, dh_agent(Agent)),
  dh_agent_properties_json(Agent, Dict),
  reply_json_dict(Dict).
% POST json *
%
% Returns 400 (Bad Request) if the request does not include an agentDefinition
% in JSON format.
dh_agent_rest(Request, _):-
  cors_enable,
  request_filter(Request, post, _/json, _), !,
  catch(
    (
      http_read_json_dict(Request, Dict),
      atom_string(AgentDefinition, Dict.agentDefinition)
    ),
    Exception,
    throw(http_reply(bad_request(Exception)))
  ),
  dh_agent_create(AgentDefinition, graph(visum)),
  reply_json_dict(json{}).



% Helpers

dh_agent_head(Substrings) -->
  html(\dh_head(['Agent'|Substrings])).


%! dh_agent_properties_json(+Agent:url, -Json:dict) is det.
%! dh_agent_properties_json(-Agent:url, -Json:dict) is nondet.

dh_agent_properties_json(Agent, json{agent:Agent, agentProperties:Dicts}):-
  dh_agent(Agent),
  findall(
    json{name:Property, type:Datatype, value:Value},
    (
      dh:dh_agent_property(Agent, Property, Value),
      rdfs_subproperty_of(Property, dho:agentProperty),
      rdf(Property, rdfs:range, Datatype, dh)
    ),
    Dicts
  ).


%! http_exists(+Request:list(nvpair), :Goal) is det.
:- meta_predicate(http_exists(+,0)).
http_exists(_, Goal):-
  Goal, !.
http_exists(Request, _):-
  http_404([], Request).

