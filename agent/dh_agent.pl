:- module(
  dh_agent,
  [
    dh_agent/1, % ?Agent:url
    dh_agent_rest/1 % +Request:list(nvpair)
  ]
).

/** <module> DataHives Agent

Interface to agents in DataHives.

@author Wouter Beek
@version 2014/09, 2014/11
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(lambda_meta)).

:- use_module(plHttp(request_ext)).

:- use_module(plHtml(html)).
:- use_module(plHtml(html_pl_term)).
:- use_module(plHtml(html_table)).

:- use_module(plTabular(rdf_html_table)).

:- use_module(dh(agent/dh_agent_create)).
:- use_module(dh(agent/dh_agent_property)).
:- use_module(dh(agent/def/dh_agent_definition)).
:- use_module(dh(core/dh_population)).
:- use_module(dh(web/dh_web_generics)).

:- meta_predicate(http_exists(+,0)).

:- dynamic(http:location/3).
:- multifile(http:location/3).

http:location(dh_agent, dh('Agent'), []).

:- http_handler(
     dh_agent(.),
     dh_agent_rest,
     [id(dhAgent),prefix,priority(-1)]
   ).





%! dh_agent(+Agent:url) is semidet.
%! dh_agent(-Agent:url) is nondet.

dh_agent(Agent):-
  rdfs_individual_of(Agent, dho:'Agent').


% Agent -> AgentDefinition
dh_agent_rest(Request):-
  memberchk(path(Path), Request),
  http_absolute_uri(Path, Location),
  once(dh_agent_definition(Location)),
  dh_agent_definition_rest_path(Request).
% GET html *
dh_agent_rest(Request):-
  cors_enable,
  request_filter(Request, get, _/html, Root),
  http_absolute_uri(dh_agent(.), Root), !,

  aggregate_all(
    set(Property),
    dh_agent_property(Property),
    Properties
  ),

  % Create a row for each agent.
  findall(
    [Agent|Values],
    (
      dh_agent(Agent),
      maplist(dh_agent_property(Agent), Properties, Values)
    ),
    Rows
  ),

  dh_population_property(dho:size, Size),
  user:current_html_style(HtmlStyle),
  reply_html_page(
    HtmlStyle,
    \dh_agent_head(['Overview']),
    \dh_body(
      \rdf_html_table(
        html([
          'Overview of the ',
          \html_pl_term(dh, Size),
          ' currently running agents in DataHives.'
        ]),
        [['Agent'|Properties]|Rows],
        [graph(dh),header_row(true),index(true),iri_display(label)]
      )
    )
  ).
% GET html PATH
% Returns 404 (Not Found) is the agent does not exist on the server.
dh_agent_rest(Request):-
  cors_enable,
  request_filter(Request, get, _/html, Agent), !,

  % Reply with a failure code if the agent does not exist.
  http_exists(Request, dh_agent(Agent)),

  aggregate_all(
    [Property,Value],
    dh_agent_property(Agent, Property, Value),
    Rows
  ),
  rdfs_label(Agent, Label),
  user:current_html_style(HtmlStyle),
  reply_html_page(
    HtmlStyle,
    \dh_agent_head([Label]),
    \dh_body(
      \html_table(
        html(['Overview of agent ', \html_link(Agent,Label), '.']),
        Rows,
        [header_row(false),index(false)]
      )
    )
  ).
% GET json *
dh_agent_rest(Request):-
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
dh_agent_rest(Request):-
  cors_enable,
  request_filter(Request, get, _/json, Agent), !,
  http_exists(Request, dh_agent(Agent)),
  dh_agent_properties_json(Agent, Dict),
  reply_json_dict(Dict).
% POST json *
%
% Returns 400 (Bad Request)
dh_agent_rest(Request):-
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





% HELPERS

dh_agent_head(Substrings) -->
  html(\dh_head(['Agent'|Substrings])).


%! dh_agent_properties_json(+Agent:url, -Json:dict) is det.
%! dh_agent_properties_json(-Agent:url, -Json:dict) is nondet.

dh_agent_properties_json(Agent, json{agent:Agent, agentProperties:Dicts}):-
  dh_agent(Agent),
  findall(
    json{name:Property, type:Datatype, value:Value},
    (
      dh_agent_property(Agent, Property, Value),
      rdf(Property, rdfs:range, Datatype, dh)
    ),
    Dicts
  ).


%! http_exists(+Request:list(nvpair), :Goal) is det.

http_exists(_, Goal):-
  Goal, !.
http_exists(Request, _):-
  http_404([], Request).

