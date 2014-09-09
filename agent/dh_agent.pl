:- module(
  dh_agent,
  [
    dh_agent/2, % +Request:list(nvpair)
                % +HtmlStyle
    dh_agent_db/1 % ?Agent:url
  ]
).

/** <module> DataHives Agent

Interface to agents in DataHives.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_cors)).
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
:- use_module(dh_core(dh_generics)). % RDF namespace.
:- use_module(dh_core(dh_population)).
:- use_module(dh_web(dh_web_generics)).



% GET html *
dh_agent(Request, HtmlStyle):-
  cors_enable,
  request_filter(Request, get, _/html, Root),
  http_absolute_uri(dh_agent(.), Root), !,
  aggregate_all(
    set(Property),
    dh_agent_property(Property),
    Properties
  ),
  findall(
    Row,
    (
      dh_agent_db(Agent),
      maplist(
        \Property^Value^dh_agent_property(Agent, Property, Value),
        Properties,
        Row
      )
    ),
    Rows
  ),
  number_of_agents(N),
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
          \html_pl_term(dh, N),
          ' currently running agents in DataHives.'
        ]),
        [HeaderRow|Rows],
        [header_row(true),index(true)]
      )
    )
  ).
% GET html PATH
dh_agent(Request, HtmlStyle):-
  cors_enable,
  request_filter(Request, get, _/html, Agent), !,
gtrace,
  findall(
    [Name,Value],
    dh_agent_property(Agent, Name, Value),
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
% POST json PATH
%
% Returns 400 (Bad Request) if the request does not include an agentDefinition
% in JSON format.
dh_agent(Request, _):-
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


%! dh_agent_db(+Agent:url) is semidet.
%! dh_agent_db(-Agent:url) is nondet.

dh_agent_db(Agent):-
  rdfs_individual_of(Agent, dh:'Agent').



% Helpers

dh_agent_head(Dcg) -->
  html(\dh_head(['Agent',Dcg])).

